use crate::compiler::value::Value;
use std::collections::HashMap;

pub trait VariableStorage {
    type Key;

    fn get(&self, key: &Self::Key) -> Option<&Value>;
    fn get_mut(&mut self, key: &Self::Key) -> Option<&mut Value>;
    fn insert(&mut self, key: Self::Key, value: Value);
    fn contains_key(&self, key: &Self::Key) -> bool;
}

struct InMemoryStorage<K> {
    inner: HashMap<K, Value>,
}

impl<K> VariableStorage for InMemoryStorage<K>
where
    K: std::hash::Hash + Eq,
{
    type Key = K;

    fn get(&self, key: &Self::Key) -> Option<&Value> {
        self.inner.get(key)
    }

    fn get_mut(&mut self, key: &Self::Key) -> Option<&mut Value> {
        self.inner.get_mut(key)
    }

    fn insert(&mut self, key: Self::Key, value: Value) {
        self.inner.entry(key).or_insert(value);
    }

    fn contains_key(&self, key: &Self::Key) -> bool {
        self.inner.contains_key(key)
    }
}
