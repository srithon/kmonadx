use super::parser::{AccessModifier, LazyButton, Map};

#[derive(Copy, Clone)]
pub struct LayerButton<'a> {
    /// What the button is mapped to
    value: &'a str,
    /// `true` if the button was inherited from a parent layer, `false` if it was explicitly set in
    /// [[keys]]
    is_inherited: bool,
}

impl<'a> LayerButton<'a> {
    /// Creates an uninherited LayerButton
    pub fn new(value: &'a str) -> LayerButton<'a> {
        LayerButton {
            value,
            is_inherited: false,
        }
    }

    /// Returns a reference to the LayerButton's value
    pub fn value(&self) -> &str {
        self.value
    }

    /// Returns `true` if the button was inherited, `false` if it was explicitly set
    pub fn is_inherited(&self) -> bool {
        self.is_inherited
    }

    /// Creates an inherited LayerButton
    pub const fn new_inherited(value: &'a str) -> LayerButton<'a> {
        LayerButton {
            value,
            is_inherited: true,
        }
    }

    /// Create a `fallthrough` button
    pub const fn fallthrough_button() -> LayerButton<'a> {
        Self::new_inherited("_")
    }

    /// Create a `break` button
    pub const fn break_button() -> LayerButton<'a> {
        Self::new_inherited("XX")
    }
}

pub struct Layer<'a, T> {
    pub keys: Vec<LayerButton<'a>>,
    pub aliases: Option<&'a Map<'a, (LazyButton<'a, T>, AccessModifier)>>,
}

// We manually implement `Clone` because the derive macro only creates an implementation for
// `&Layer`, whose `clone` method would then take `&&self`, returning another `&Layer`.
// It (maybe) does this because at the call-site, `T` is not cloneable
impl<'a, T> Clone for Layer<'a, T> {
    fn clone(&self) -> Self {
        let keys = self.keys.clone();
        let aliases = self.aliases.clone();

        Layer::new(keys, aliases)
    }
}

impl<'a, T> Layer<'a, T> {
    pub fn new(
        keys: Vec<LayerButton<'a>>,
        aliases: Option<&'a Map<'a, (LazyButton<'a, T>, AccessModifier)>>,
    ) -> Layer<'a, T> {
        Layer { keys, aliases }
    }

    /// Inherit `keys` from `parent` according to the following rules:
    ///
    /// `parent::key` and `self::key` are inherited:
    /// nothing happens
    ///
    /// `self::key` is inherited but `parent::key` is not:
    /// `self::key` = `parent::key`
    ///
    /// `parent::key` is inherited but `self::key` is not:
    /// nothing happens
    pub fn inherit(&mut self, parent: &Self) {
        for (old_key, parent_key) in self.keys.iter_mut().zip(parent.keys.iter()) {
            if !parent_key.is_inherited() {
                if old_key.is_inherited() {
                    *old_key = *parent_key;
                }
            }
        }
    }
}
