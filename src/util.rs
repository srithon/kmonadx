const STRING_STACK_INITIAL_STRING_CAPACITY: usize = 80;
const STRING_STACK_INITIAL_LAYER_CAPACITY: usize = 5;

/// A stack of strings represented as a single String internally.
/// Unlike with a Vec<&str>, it can yield a regular &str in O(1) time
pub(crate) struct StringStack {
    string: String,
    positions: Vec<usize>,
}

impl StringStack {
    /// Creates a new StringStack with default allocations
    pub fn new() -> StringStack {
        StringStack {
            string: String::with_capacity(STRING_STACK_INITIAL_STRING_CAPACITY),
            positions: Vec::with_capacity(STRING_STACK_INITIAL_LAYER_CAPACITY),
        }
    }

    /// Pushes a string onto the stack
    pub fn push(&mut self, new_segment: &str) {
        self.positions.push(self.total_length());
        self.string.push_str(new_segment)
    }

    /// Returns true if something was popped, false otherwise
    pub fn pop(&mut self) -> bool {
        if let Some(position) = self.positions.pop() {
            self.string.truncate(position);
            true
        } else {
            false
        }
    }

    /// Combines the last two segments on the stack. Returns true if there were two segments to
    /// combine, false otherwise
    pub fn merge_last(&mut self) -> bool {
        let num_segments = self.num_segments();

        if num_segments >= 2 {
            // want the last segment to be continuous, so we remove the marker indicating the start
            // of the last segment. by doing so, we add the contents of that segment to what used
            // to be the second-to-last segment.
            self.positions.remove(num_segments - 1);
            true
        } else {
            false
        }
    }

    /// Clears the stack
    pub fn clear(&mut self) {
        self.string.clear()
    }

    /// Returns the number of substrings compose the stack
    pub fn num_segments(&self) -> usize {
        self.positions.len()
    }

    /// Returns the length of the internal string
    pub fn total_length(&self) -> usize {
        self.string.len()
    }

    /// Returns true if the internal string is empty, otherwise false
    pub fn is_empty(&self) -> bool {
        self.string.is_empty()
    }

    /// Returns a reference to the internal String
    pub fn as_str(&self) -> &str {
        &self.string
    }
}

mod tests {
    use super::StringStack;

    #[test]
    fn empty_pop_test() {
        let mut stack = StringStack::new();
        stack.pop();
        stack.pop();
    }

    #[test]
    fn push_pop_test() {
        let mut stack = StringStack::new();
        assert_eq!(stack.as_str(), "");
        stack.push("hello");
        assert_eq!(stack.as_str(), "hello");
        stack.push("test");
        assert_eq!(stack.as_str(), "hellotest");
        stack.push("12345");
        assert_eq!(stack.as_str(), "hellotest12345");
        stack.pop();
        assert_eq!(stack.as_str(), "hellotest");
        stack.pop();
        assert_eq!(stack.as_str(), "hello");
        stack.pop();
        assert_eq!(stack.as_str(), "");
    }

    #[test]
    fn num_segments_test() {
        let mut stack = StringStack::new();
        assert_eq!(stack.num_segments(), 0);
        stack.push("1");
        assert_eq!(stack.num_segments(), 1);
        stack.push("2");
        assert_eq!(stack.num_segments(), 2);
        stack.push("3");
        assert_eq!(stack.num_segments(), 3);
    }
}
