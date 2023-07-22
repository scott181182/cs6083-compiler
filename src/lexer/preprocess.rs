
/**
 * This enumerates the state for the comment stripping state machine.
 * Since some of these states are parameterized, this is technically and Pushdown Automata instead of a Finate State Machine.
 */
enum WhitespaceStates {
    /// Normal state, where characters are added to the output stream.
    /// Checks for any slash characters that could signal the start of a comment.
    Normal,
    /// Normal state, but a slash was the last character parsed.
    /// If the next character is a slash, goto SingleLineComment,
    /// if the next character is an asterisk, goto MultiLineComment(1),
    /// otherwise, add the character to the output (with a slash to make up for the last character that was skipped).
    Slash,
    /// Inside a single-line comment. Skip all characters until a newline (\n), which will return us to the Normal state.
    SingleLineComment,
    /// Inside `n` multiline comments (to track nested comments).
    /// Will check for slashes and asterisks that may increase or decrease our nesting level.
    MultiLineComment(u32),
    /// Inside a multiline comment, but the last character was a slash.
    /// If the next character is an asterisk, return to MultiLineComment with an increased depth,
    /// otherwise return to MultiLineComment with the same depth.
    MultiLineCommentSlash(u32),
    /// Inside a multiline comment, but the last character was an asterisk.
    /// If the next character is a slash, return to MultiLineComment with a decreased depth,
    /// otherwise return to MultiLineComment with the same depth.
    /// If the decreased depth is zero, return to the Normal state.
    MultiLineCommentStar(u32)
}



pub fn strip_comments(raw_content: String) -> String {
    let mut ret = String::with_capacity(raw_content.len());

    let mut state = WhitespaceStates::Normal;

    for c in raw_content.chars() {
        state = match (state, c) {
            (WhitespaceStates::Normal, '/') => WhitespaceStates::Slash,
            (WhitespaceStates::Normal, _) => {
                ret.push(c);
                WhitespaceStates::Normal
            },

            (WhitespaceStates::Slash, '/') => WhitespaceStates::SingleLineComment,
            (WhitespaceStates::Slash, '*') => WhitespaceStates::MultiLineComment(1),
            (WhitespaceStates::Slash, _) => {
                ret.push('/');
                ret.push(c);
                WhitespaceStates::Normal
            },

            (WhitespaceStates::SingleLineComment, '\n') => {
                ret.push(c);
                WhitespaceStates::Normal
            },
            (WhitespaceStates::SingleLineComment, _) => { WhitespaceStates::SingleLineComment },


            (WhitespaceStates::MultiLineComment(n), '/') => WhitespaceStates::MultiLineCommentSlash(n),
            (WhitespaceStates::MultiLineComment(n), '*') => WhitespaceStates::MultiLineCommentStar(n),
            (WhitespaceStates::MultiLineComment(n), _) => WhitespaceStates::MultiLineComment(n),

            (WhitespaceStates::MultiLineCommentSlash(n), '*') => WhitespaceStates::MultiLineComment(n + 1),
            (WhitespaceStates::MultiLineCommentSlash(n), _) => WhitespaceStates::MultiLineComment(n),

            (WhitespaceStates::MultiLineCommentStar(n), '/') =>
                if n == 1 { WhitespaceStates::Normal } else { WhitespaceStates::MultiLineComment(n - 1) },
            (WhitespaceStates::MultiLineCommentStar(n), _) => WhitespaceStates::MultiLineComment(n),
        }
    }

    return ret;
}