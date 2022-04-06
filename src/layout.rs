//! An implemention of a basic layouting engine, similar to bevy UI but more concise and practical
//! for the elements that are already used and usable in any 2D setup.

use bevy::prelude::*;
use bevy::text::Text2dSize;
use bevy::utils::HashMap;

/// A plugin for automatically arrange entities in containers.
///
/// See [LayoutContainerBundle] and [UiSize] for the necessary components.
pub(crate) struct LayoutPlugin;

impl Plugin for LayoutPlugin {
    fn build(&self, app: &mut App) {
        app.add_system_set(
            SystemSet::new()
                .with_system(LayoutPlugin::update_text_size)
                .before("reflow"),
        )
        .add_system(LayoutPlugin::reflow_containers.label("reflow"));
    }
}

impl LayoutPlugin {
    /// Set the UiSize components to the calculated text sizes.
    ///
    /// This is necessary because the entity size is not trivial to calculate as it is dependent on
    /// font, ligatures, font size, transform, etc.
    fn update_text_size(mut texts: Query<(&Text2dSize, &mut UiSize), Changed<Text2dSize>>) {
        for (updated, mut size) in texts.iter_mut() {
            size.0 = updated.size;
        }
    }

    /// Resize parents to fit all children and reposition children to be inside the parent and not
    /// overlap each other.
    ///
    /// # Parameters
    ///
    /// `cache` is a container-size cache that also holds a flag whether the container is a root
    /// in the layout tree or a child.
    fn reflow_containers(
        containers: Query<(
            Entity,
            &LayoutChildren,
            Option<&Margin>,
            Option<&Padding>,
            Option<&Spacing>,
        )>,
        mut sizes: Query<&mut UiSize>,
        mut transforms: Query<&mut Transform>,
        mut cache: Local<HashMap<Entity, (UiSize, bool)>>,
    ) {
        /// Recursively recalculate the tree of entity sizes with caching.
        fn calc_child_size(
            child: Entity,
            containers: &Query<(
                Entity,
                &LayoutChildren,
                Option<&Margin>,
                Option<&Padding>,
                Option<&Spacing>,
            )>,
            sizes: &mut Query<&mut UiSize>,
            cache: &mut Local<HashMap<Entity, (UiSize, bool)>>,
        ) -> UiSize {
            let (container, LayoutChildren(children), margin, padding, spacing) =
                containers.get(child).unwrap();
            let margin: Rect<LayoutUnit> = margin.map(|Margin(rect)| *rect).unwrap_or_default();
            let padding: Rect<LayoutUnit> = padding.map(|Padding(rect)| *rect).unwrap_or_default();
            let spacing: LayoutUnit = spacing.map(|Spacing(s)| *s).unwrap_or_default();
            let child_sizes = children
                .iter()
                .map(|c| match cache.get_mut(c) {
                    Some((cached, is_root)) => {
                        *is_root = false;
                        *cached
                    }
                    None => {
                        if containers.contains(*c) {
                            calc_child_size(*c, &containers, sizes, cache)
                        } else {
                            get_leaf_size(*c, sizes)
                        }
                    }
                })
                .collect::<Vec<_>>();
            let num_spaces = (child_sizes.len() as LayoutUnit - LayoutUnit::from(1u8))
                .max(LayoutUnit::from(0u8));
            let width = margin.left
                + margin.right
                + padding.left
                + padding.right
                + child_sizes.iter().map(|s| s.0.width).sum::<LayoutUnit>()
                + num_spaces * spacing;
            let height = margin.top
                + margin.bottom
                + padding.top
                + padding.bottom
                + child_sizes.iter().map(|s| s.0.height).sum::<LayoutUnit>();
            let size = UiSize(Size::new(width, height));
            let is_root = true;
            cache.insert(container, (size.clone(), is_root));
            size
        }
        /// A small wrapper over leaf-only lookups that contains a design decision of how to handle
        /// missing components.
        fn get_leaf_size(e: Entity, sizes: &Query<&mut UiSize>) -> UiSize {
            // Silently assume zero size for malformed entities.
            sizes.get(e).copied().unwrap_or_default()
        }
        /// Get a size from the cache or leaf element without calculating it.
        /// Also does not reset the root flag.
        fn get_size(
            e: Entity,
            sizes: &Query<&mut UiSize>,
            cache: &Local<HashMap<Entity, (UiSize, bool)>>,
        ) -> UiSize {
            cache
                .get(&e)
                .map(|(cached, _)| *cached)
                .unwrap_or_else(|| get_leaf_size(e, sizes))
        }
        fn is_root(e: &Entity, cache: &Local<HashMap<Entity, (UiSize, bool)>>) -> bool {
            cache.get(e).unwrap().1
        }
        /// Recursively set the positions of the entity with a given offset.
        fn set_position(
            entity: Entity,
            mut offset: Vec3,
            containers: &Query<(
                Entity,
                &LayoutChildren,
                Option<&Margin>,
                Option<&Padding>,
                Option<&Spacing>,
            )>,
            sizes: &Query<&mut UiSize>,
            mut transforms: &mut Query<&mut Transform>,
            cache: &Local<HashMap<Entity, (UiSize, bool)>>,
        ) {
            // Set position
            if let Ok(mut t) = transforms.get_mut(entity) {
                t.translation = offset;
            } else {
                eprintln!("failed to reposition {:?}. No Transform component.", entity)
            }
            // Recursively set child positions.
            if let Some((_, LayoutChildren(children), margin, padding, spacing)) =
                containers.get(entity).ok()
            {
                let margin: Rect<LayoutUnit> = margin.map(|Margin(rect)| *rect).unwrap_or_default();
                let padding: Rect<LayoutUnit> =
                    padding.map(|Padding(rect)| *rect).unwrap_or_default();
                let spacing: LayoutUnit = spacing.map(|Spacing(s)| *s).unwrap_or_default();

                offset.x += margin.left + padding.left;
                offset.y += margin.top + padding.top;

                for child in children {
                    set_position(*child, offset, &containers, &sizes, &mut transforms, &cache);
                    let size = get_size(*child, &sizes, &cache).0;
                    offset.x += size.width;
                    offset.y += size.height + spacing;
                }
            }
        }

        // Keep a `Local` cache to reduce memory allocations and repeated growing.
        cache.clear();

        // Resize containers.
        for container in containers.iter() {
            let entity = container.0;
            if !cache.contains_key(&entity) {
                let size = calc_child_size(entity, &containers, &mut sizes, &mut cache);
                if let Ok(mut target) = sizes.get_mut(entity) {
                    *target = size;
                }
            }
        }

        // Reposition all elements.
        for container in containers.iter() {
            let this = container.0;
            if is_root(&this, &cache) {
                set_position(
                    this,
                    Vec3::new(0.0, 0.0, 0.0),
                    &containers,
                    &sizes,
                    &mut transforms,
                    &cache,
                );
            }
        }
    }
}

/// All components for a container to work with the layout systems.
#[derive(Debug, Default, Clone, Bundle)]
pub(crate) struct LayoutContainerBundle {
    pub(crate) children: LayoutChildren,
    pub(crate) margin: Margin,
    pub(crate) padding: Padding,
    pub(crate) spacing: Spacing,
    pub(crate) size: UiSize,
}

/// A component for defining a parent entity for the layout system.
///
/// Having this component makes an entity a *Container* instead of a simple leaf element.
#[derive(Debug, Default, Clone, Component)]
pub(crate) struct LayoutChildren(pub(crate) Vec<Entity>);

/// The size of an entity with all children.
///
/// # Note
///
/// All leaf elements keep their given size. The default is set to (0,0) because containers are
/// resized automatically, leaf elements are not.
#[derive(Debug, Default, Copy, Clone, Component)]
pub(crate) struct UiSize(pub(crate) Size<LayoutUnit>);

/// An abstract unit for measurements in context of the layout module.
pub(crate) type LayoutUnit = f32;

/// The amount of clear space outside the container on each side.
#[derive(Debug, Default, Copy, Clone, Component)]
pub(crate) struct Margin(pub(crate) Rect<LayoutUnit>);

/// The amount of clear space inside the container on each side.
#[derive(Debug, Default, Copy, Clone, Component)]
pub(crate) struct Padding(pub(crate) Rect<LayoutUnit>);

/// The amount of clear space between elements in a container.
#[derive(Debug, Default, Copy, Clone, Component)]
pub(crate) struct Spacing(pub(crate) LayoutUnit);
