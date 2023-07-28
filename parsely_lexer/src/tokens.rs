#[macro_export]
macro_rules! define_punctuation {
    ($vis:vis enum $token_enum:ident { $($struct_name:ident = $st:expr),*, $(,)* }) => {
        $(
            #[derive(Debug)]
            $vis struct $struct_name($crate::Span);

            impl $struct_name {
                pub fn from_span_start(start: $crate::Position) -> Token {
                    Token::$struct_name($struct_name($crate::Span {
                        end: $crate::Position {
                            line: start.line,
                            column: start.column + $st.len(),
                        },
                        start,
                    }))
                }
            }
        )*
        #[derive(Debug)]
        $vis enum $token_enum {
           $(
                $struct_name($struct_name)
           ),*
        }
    };
}