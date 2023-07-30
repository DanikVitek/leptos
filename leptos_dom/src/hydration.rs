#[cfg(all(
    feature = "islands",
    any(feature = "hydrate", feature = "ssr")
))]
use leptos_reactive::SharedContext;
use std::{cell::RefCell, fmt::Display};

#[cfg(all(target_arch = "wasm32", feature = "hydrate"))]
mod hydration {
    use once_cell::unsync::Lazy as LazyCell;
    use std::{cell::RefCell, collections::HashMap};
    use wasm_bindgen::JsCast;

    // We can tell if we start in hydration mode by checking to see if the
    // id "_0-1" is present in the DOM. If it is, we know we are hydrating from
    // the server, if not, we are starting off in CSR
    thread_local! {
      pub static HYDRATION_COMMENTS: LazyCell<HashMap<String, web_sys::Comment>> = LazyCell::new(|| {
        let document = crate::document();
        let body = document.body().unwrap();
        let walker = document
          .create_tree_walker_with_what_to_show(&body, 128)
          .unwrap();
        let mut map = HashMap::new();
        while let Ok(Some(node)) = walker.next_node() {
          if let Some(content) = node.text_content() {
            if let Some(hk) = content.strip_prefix("hk=") {
              if let Some(hk) = hk.split('|').next() {
                map.insert(hk.into(), node.unchecked_into());
              }
            }
          }
        }
        map
      });

      #[cfg(debug_assertions)]
      pub static VIEW_MARKERS: LazyCell<HashMap<String, web_sys::Comment>> = LazyCell::new(|| {
        let document = crate::document();
        let body = document.body().unwrap();
        let walker = document
          .create_tree_walker_with_what_to_show(&body, 128)
          .unwrap();
        let mut map = HashMap::new();
        while let Ok(Some(node)) = walker.next_node() {
          if let Some(content) = node.text_content() {
            if let Some(id) = content.strip_prefix("leptos-view|") {
              map.insert(id.into(), node.unchecked_into());
            }
          }
        }
        map
      });

      pub static IS_HYDRATING: RefCell<LazyCell<bool>> = RefCell::new(LazyCell::new(|| {
        #[cfg(debug_assertions)]
        return crate::document().get_element_by_id("_0-0-1").is_some()
          || crate::document().get_element_by_id("_0-0-1o").is_some()
          || HYDRATION_COMMENTS.with(|comments| comments.get("_0-0-1o").is_some());

        #[cfg(not(debug_assertions))]
        return crate::document().get_element_by_id("_0-0-1").is_some()
          || HYDRATION_COMMENTS.with(|comments| comments.get("_0-0-1").is_some());
      }));
    }

    pub fn get_marker(id: &str) -> Option<web_sys::Comment> {
        HYDRATION_COMMENTS.with(|comments| comments.get(id).cloned())
    }
}

#[cfg(all(target_arch = "wasm32", feature = "hydrate"))]
pub(crate) use hydration::*;

/// A stable identifier within the server-rendering or hydration process.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct HydrationKey {
    /// ID of the current fragment.
    pub fragment: usize,
    /// ID of the current error boundary.
    pub error: usize,
    /// ID of the current key.
    pub id: usize,
}

impl Display for HydrationKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}-{}", self.fragment, self.error, self.id)
    }
}

impl std::str::FromStr for HydrationKey {
    type Err = (); // TODO better error

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut pieces = s.splitn(3, '-');
        let first = pieces.next().ok_or(())?;
        let second = pieces.next().ok_or(())?;
        let third = pieces.next().ok_or(())?;
        let fragment = usize::from_str(first).map_err(|_| ())?;
        let error = usize::from_str(second).map_err(|_| ())?;
        let id = usize::from_str(third).map_err(|_| ())?;
        Ok(HydrationKey {
            fragment,
            error,
            id,
        })
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn parse_hydration_key() {
        use crate::HydrationKey;
        use std::str::FromStr;
        assert_eq!(
            HydrationKey::from_str("1-2-3"),
            Ok(HydrationKey {
                fragment: 1,
                error: 2,
                id: 3
            })
        )
    }
}

thread_local!(static ID: RefCell<HydrationKey> = RefCell::new(HydrationKey { fragment: 0, error: 0, id: 0 }));

/// Control and utility methods for hydration.
pub struct HydrationCtx;

impl HydrationCtx {
    /// If you're in an hydration context, get the next `id` without incrementing it.
    pub fn peek() -> Option<HydrationKey> {
        #[cfg(all(
            feature = "islands",
            any(feature = "hydrate", feature = "ssr")
        ))]
        let no_hydrate = SharedContext::no_hydrate();
        #[cfg(not(all(
            feature = "islands",
            any(feature = "hydrate", feature = "ssr")
        )))]
        let no_hydrate = false;
        if no_hydrate {
            None
        } else {
            Some(ID.with(|id| *id.borrow()))
        }
    }

    /// Get the next `id` without incrementing it.
    pub fn peek_always() -> HydrationKey {
        ID.with(|id| *id.borrow())
    }

    /// Increments the current hydration `id` and returns it
    pub fn id() -> Option<HydrationKey> {
        #[cfg(all(
            feature = "islands",
            any(feature = "hydrate", feature = "ssr")
        ))]
        let no_hydrate = SharedContext::no_hydrate();
        #[cfg(not(all(
            feature = "islands",
            any(feature = "hydrate", feature = "ssr")
        )))]
        let no_hydrate = false;

        if no_hydrate {
            None
        } else {
            Some(ID.with(|id| {
                let mut id = id.borrow_mut();
                id.id = id.id.wrapping_add(1);
                *id
            }))
        }
    }

    /// Resets the hydration `id` for the next component, and returns it
    pub fn next_component() -> HydrationKey {
        ID.with(|id| {
            let mut id = id.borrow_mut();
            id.fragment = id.fragment.wrapping_add(1);
            id.id = 0;
            *id
        })
    }

    /// Resets the hydration `id` for the next component, and returns it
    pub fn next_error() -> HydrationKey {
        ID.with(|id| {
            let mut id = id.borrow_mut();
            id.error = id.error.wrapping_add(1);
            id.id = 0;
            *id
        })
    }

    #[doc(hidden)]
    #[cfg(not(all(target_arch = "wasm32", feature = "web")))]
    pub fn reset_id() {
        ID.with(|id| {
            *id.borrow_mut() = HydrationKey {
                fragment: 0,
                error: 0,
                id: 0,
            }
        });
    }

    /// Resumes hydration from the provided `id`. Useful for
    /// `Suspense` and other fancy things.
    pub fn continue_from(id: HydrationKey) {
        ID.with(|i| *i.borrow_mut() = id);
    }

    /// Resumes hydration after the provided `id`. Useful for
    /// islands and other fancy things.
    pub fn continue_after(id: HydrationKey) {
        ID.with(|i| {
            *i.borrow_mut() = HydrationKey {
                fragment: id.fragment,
                error: id.error,
                id: id.id + 1,
            }
        });
    }

    #[cfg(all(target_arch = "wasm32", feature = "web"))]
    #[doc(hidden)]
    pub fn stop_hydrating() {
        #[cfg(feature = "hydrate")]
        {
            IS_HYDRATING.with(|is_hydrating| {
                std::mem::take(&mut *is_hydrating.borrow_mut());
            })
        }
    }

    #[doc(hidden)]
    #[cfg(all(feature = "hydrate", target_arch = "wasm32", feature = "web"))]
    pub fn with_hydration_on<T>(f: impl FnOnce() -> T) -> T {
        use once_cell::unsync::Lazy as LazyCell;

        let prev = IS_HYDRATING.with(|is_hydrating| {
            std::mem::take(&mut *is_hydrating.borrow_mut())
        });
        IS_HYDRATING.with(|is_hydrating| {
            *is_hydrating.borrow_mut() = LazyCell::new(|| true)
        });
        let value = f();
        IS_HYDRATING.with(|is_hydrating| *is_hydrating.borrow_mut() = prev);
        value
    }

    /// Whether the UI is currently in the process of hydrating from the server-sent HTML.
    pub fn is_hydrating() -> bool {
        #[cfg(all(target_arch = "wasm32", feature = "hydrate"))]
        {
            IS_HYDRATING.with(|is_hydrating| **is_hydrating.borrow())
        }
        #[cfg(not(all(target_arch = "wasm32", feature = "hydrate")))]
        {
            false
        }
    }

    #[cfg(feature = "hydrate")]
    pub(crate) fn to_string(id: &HydrationKey, closing: bool) -> String {
        #[cfg(debug_assertions)]
        return format!("_{id}{}", if closing { 'c' } else { 'o' });

        #[cfg(not(debug_assertions))]
        {
            let _ = closing;

            format!("_{id}")
        }
    }
}
