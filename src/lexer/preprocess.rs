
enum WhitespaceStates {
    Normal,
    Slash,
    SingleLineComment,
    MultiLineComment(u32),
    MultiLineCommentSlash(u32),
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