//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use logos::{Logos, Lexer};

macro_rules! reserved {
    ( $e:expr , $( $match:expr ),* ) => {
        match $e {
            $(
                $match => false,
            )*
                _ => true,
        }
    };
}

fn ident(lex: &mut Lexer<'_, TokenKind>) -> bool {
    // Some of these are used by DML, others are reserved in C or C++
    reserved!(lex.slice(),
              // dml reserved
              "attribute", "auto", "bank", "bitorder", "connect", "constant",
              "data", "device", "event", "field", "footer", "group", "header",
              "implement", "import", "interface", "independent", "loggroup",
              "memoized", "method", "port", "size", "startup", "throws",
              "_header", "after", "assert", "bitfields", "call", "cast",
              "defined", "error", "foreach", "in", "is", "layout", "local",
              "log", "select", "sizeoftype", "then", "typeof", "undefined",
              "vect", "_warning", "where", "each", "session", "sequence",
              "subdevice", "provisional",
              // reserved from C
              "break", "case", "char", "const", "continue", "default",
              "do", "double", "else", "enum", "extern", "float", "for", "goto",
              "if", "int", "long", "register", "return", "short", "signed",
              "sizeof", "static", "struct", "switch", "typedef", "union",
              "unsigned", "void", "volatile", "while",
              "delete", "inline", "new", "restrict", "template", "throw", "try",
              "catch", "this",
              // reserved from C++, speculatively
              "class", "namespace", "private", "protected", "public", "using",
              "virtual")
}

fn string_constant(_lex: &mut Lexer<'_, TokenKind>) -> bool {
    // TODO: check string utf-8 validity
    true
}

fn char_constant(_lex: &mut Lexer<'_, TokenKind>) -> bool {
    // TODO: check character utf-8 validity
    true
}

fn handle_multiline_comment(lex: &mut Lexer<'_, TokenKind>) -> TokenKind {
    if lex.slice() == "/*" {
        loop {
            if lex.remainder().starts_with('*') {
                lex.bump('*'.len_utf8());
                if lex.remainder().starts_with('/') {
                    lex.bump('/'.len_utf8());
                    break;
                }
            } else {
                let next_char = &lex.remainder().chars().next();
                if let Some(c) = next_char {
                    lex.bump(c.len_utf8());
                } else {
                    return TokenKind::LexerError;
                }
            }
        }
    }
    TokenKind::MultilineComment
}

fn handle_cblock(lex: &mut Lexer<'_, TokenKind>) -> TokenKind {
    if lex.slice() == "%{" {
        loop {
            if lex.remainder().starts_with('%') {
                lex.bump('%'.len_utf8());
                if lex.remainder().starts_with('}') {
                    lex.bump('}'.len_utf8());
                    break;
                }
            } else {
                let next_char = &lex.remainder().chars().next();
                if let Some(c) = next_char {
                    lex.bump(c.len_utf8());
                } else {
                    return TokenKind::LexerError;
                }
            }
        }
    }
    TokenKind::CBlock
}

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
pub enum TokenKind {
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("%")]
    Mod,
    #[token("|")]
    BinOr,
    #[token("&")]
    BinAnd,
    #[token("~")]
    BinNot,
    #[token("^")]
    BinXor,
    #[token("<<")]
    LShift,
    #[token(">>")]
    RShift,
    #[token("||")]
    Or,
    #[token("&&")]
    And,
    #[token("!")]
    Not,
    #[token("==")]
    Equals,
    #[token(">")]
    GreaterThan,
    #[token("<")]
    LessThan,
    #[token("!=")]
    NotEquals,
    #[token(">=")]
    GEquals,
    #[token("<=")]
    LEquals,
    #[token("=")]
    Assign,
    #[token("*=")]
    TimesAssign,
    #[token("/=")]
    DivideAssign,
    #[token("%=")]
    ModAssign,
    #[token("+=")]
    PlusAssign,
    #[token("-=")]
    MinusAssign,
    #[token("<<=")]
    LShiftAssign,
    #[token(">>=")]
    RShiftAssign,
    #[token("&=")]
    BAndAssign,
    #[token("^=")]
    BXorAssign,
    #[token("|=")]
    BOrAssign,
    #[token("++")]
    PlusPlus,
    #[token("--")]
    MinusMinus,
    #[token("->")]
    Arrow,
    #[token("?")]
    CondOp,
    #[token(":")]
    Colon,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(";")]
    SemiColon,
    #[token("...")]
    Ellipsis,
    #[token("@")]
    At,
    #[token("#if")]
    HashIf,
    #[token("#else")]
    HashElse,
    #[token("#foreach")]
    HashForeach,
    #[token("#select")]
    HashSelect,
    #[token("#?")]
    HashCondOp,
    #[token("#:")]
    HashColon,
    #[token(r"%{", handle_cblock)]
    CBlock,
    #[token("after")]
    After,
    #[token("as")]
    As,
    #[token("assert")]
    Assert,
    #[token("async")]
    Async,
    #[token("attribute")]
    Attribute,
    #[token("auto")]
    Auto, // We do not expect this in user files
    #[token("await")]
    Await,
    #[token("bank")]
    Bank,
    #[token("bitfields")]
    Bitfields,
    #[token("bitorder")]
    Bitorder,
    #[token("break")]
    Break,
    #[token("call")]
    Call,
    #[token("case")]
    Case,
    #[token("cast")]
    Cast,
    #[token("catch")]
    Catch,
    #[token("char")]
    Char,
    #[token("connect")]
    Connect,
    #[token("const")]
    Const,
    #[token("constant")]
    Constant,
    #[token("continue")]
    Continue,
    #[token("data")]
    Data,
    #[token("default")]
    Default,
    #[token("defined")]
    Defined,
    #[token("delete")]
    Delete,
    #[token("device")]
    Device,
    #[token("do")]
    Do,
    #[token("double")]
    Double,
    #[token("dml")]
    DML,
    #[token("each")]
    Each,
    #[token("else")]
    Else,
    #[token("error")]
    Error,
    #[token("export")]
    Export,
    #[token("extern")]
    Extern,
    #[token("event")]
    Event,
    #[token("field")]
    Field,
    #[token("float")]
    Float,
    #[token("footer")]
    Footer,
    #[token("for")]
    For,
    #[token("foreach")]
    Foreach,
    #[token("group")]
    Group,
    #[token("header")]
    Header,
    #[token("hook")]
    Hook,
    #[token("inline")]
    Inline,
    #[token("implement")]
    Implement,
    #[token("import")]
    Import,
    #[token("if")]
    If,
    #[token("in")]
    In,
    #[token("independent")]
    Independent,
    #[token("int")]
    Int,
    #[token("is")]
    Is,
    #[token("interface")]
    Interface,
    #[token("layout")]
    Layout,
    #[token("loggroup")]
    Loggroup,
    #[token("local")]
    Local,
    #[token("log")]
    Log,
    #[token("long")]
    Long,
    #[token("memoized")]
    Memoized,
    #[token("method")]
    Method,
    #[token("new")]
    New,
    #[token("param")]
    Param,
    #[token("port")]
    Port,
    #[token("provisional")]
    Provisional,
    #[token("register")]
    Register,
    #[token("return")]
    Return,
    #[token("saved")]
    Saved,
    #[token("select")]
    Select,
    #[token("session")]
    Session,
    #[token("sequence")]
    Sequence,
    #[token("shared")]
    Shared,
    #[token("short")]
    Short,
    #[token("signed")]
    Signed,
    #[token("size")]
    Size,
    #[token("sizeof")]
    SizeOf,
    #[token("sizeoftype")]
    SizeOfType,
    #[token("static")]
    Static,
    #[token("startup")]
    Startup,
    #[token("stringify")]
    Stringify,
    #[token("struct")]
    Struct,
    #[token("subdevice")]
    Subdevice,
    #[token("switch")]
    Switch,
    #[token("template")]
    Template,
    #[token("then")]
    Then,
    #[token("this")]
    This,
    #[token("throw")]
    Throw,
    #[token("throws")]
    Throws,
    #[token("try")]
    Try,
    #[token("typedef")]
    Typedef,
    #[token("typeof")]
    TypeOf,
    #[token("undefined")]
    Undefined,
    #[token("unsigned")]
    Unsigned,
    #[token("vect")]
    Vect,
    #[token("void")]
    Void,
    #[token("where")]
    Where,
    #[token("while")]
    While,
    #[token("with")]
    With,
    // Logos is finicky with its regular expressions. Make sure any regular
    // expression in the lexer can be disambiguated from all others
    // with lookahead 1. Or use specialized callbacks in cases where logos
    // cannot handle it
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*", ident)]
    Identifier,
    #[regex(r"0x(_*[0-9a-fA-F])+")]
    HexConstant,
    #[regex(r"0b((_+[01])|[01])+")]
    BinaryConstant,
    #[regex(r"[0-9]((_+[0-9])|[0-9])*")]
    IntConstant,
    #[regex(r"[0-9]+(\.[0-9]+([eE]-?[0-9]+)?|([eE]-?[0-9]+))")]
    FloatConstant,
    #[regex(r#""([^\x00-\x1f\x7f"\\]|(\\([abefnrstv\\'"]|(x[0-9a-fA-F][0-9a-fA-F])|([0-7][0-7]?[0-7]?))))*""#,
            string_constant)]
    StringConstant,
    #[regex(r"'([^\x00-\x1f\x7f-\xff'\\]|\\.)[^']*'", char_constant)]
    CharConstant,
    #[regex(r"\n")]
    Newline,
    #[regex(r"[\t\r ]+")]
    Whitespace,
    #[regex(r"(//[^\n]*\n?)", allow_greedy=true)]
    Comment,
    #[regex(r"(/\*)", handle_multiline_comment)]
    MultilineComment,
    LexerError,
}

impl TokenKind {
    pub fn description(self) -> &'static str {
        match self {
            TokenKind::Plus => "'+'",
            TokenKind::Minus => "'-'",
            TokenKind::Multiply => "'*'",
            TokenKind::Divide => "'/'",
            TokenKind::Mod => "'%'",
            TokenKind::BinOr => "'|'",
            TokenKind::BinAnd => "'&'",
            TokenKind::BinNot => "'~'",
            TokenKind::BinXor => "'^'",
            TokenKind::LShift => "'<<'",
            TokenKind::RShift => "'>>'",
            TokenKind::Or => "'||'",
            TokenKind::And => "'&&'",
            TokenKind::Not => "'!'",
            TokenKind::Equals => "'=='",
            TokenKind::GreaterThan => "'>'",
            TokenKind::LessThan => "'<'",
            TokenKind::NotEquals => "'!'=",
            TokenKind::GEquals => "'=>'",
            TokenKind::LEquals => "'<='",
            TokenKind::Assign => "'='",
            TokenKind::TimesAssign => "'*='",
            TokenKind::DivideAssign => "'/='",
            TokenKind::ModAssign => "'%='",
            TokenKind::PlusAssign => "'+='",
            TokenKind::MinusAssign => "'-='",
            TokenKind::LShiftAssign => "'<<='",
            TokenKind::RShiftAssign => "'>>='",
            TokenKind::BAndAssign => "'&='",
            TokenKind::BXorAssign => "'^='",
            TokenKind::BOrAssign => "'|='",
            TokenKind::PlusPlus => "'++'",
            TokenKind::MinusMinus => "'--'",
            TokenKind::Arrow => "'->'",
            TokenKind::CondOp => "'?'",
            TokenKind::Colon => "':'",
            TokenKind::LParen => "'('",
            TokenKind::RParen => "')'",
            TokenKind::LBracket => "'['",
            TokenKind::RBracket => "']'",
            TokenKind::LBrace => "'{'",
            TokenKind::RBrace => "'}'",
            TokenKind::Dot =>  "'.'",
            TokenKind::Comma => "','",
            TokenKind::SemiColon => "';'",
            TokenKind::Ellipsis => "'...'",
            TokenKind::At => "'@'",
            TokenKind::HashIf => "'#if'",
            TokenKind::HashElse => "'#else'",
            TokenKind::HashForeach => "'#foreach'",
            TokenKind::HashSelect => "'#select'",
            TokenKind::HashCondOp => "'#?'",
            TokenKind::HashColon => "'#:'",
            TokenKind::CBlock => "C block",
            TokenKind::After => "'after'",
            TokenKind::As => "'as'",
            TokenKind::Assert => "'assert'",
            TokenKind::Async => "'async'",
            TokenKind::Attribute => "'attribute'",
            TokenKind::Auto => "'auto'",
            TokenKind::Await => "'await'",
            TokenKind::Bank => "'bank'",
            TokenKind::Bitfields => "'bitfields'",
            TokenKind::Bitorder => "'bitorder'",
            TokenKind::Break => "'break'",
            TokenKind::Call => "'call'",
            TokenKind::Case => "'case'",
            TokenKind::Cast => "'cast'",
            TokenKind::Catch => "'catch'",
            TokenKind::Char => "'char'",
            TokenKind::Connect => "'connect'",
            TokenKind::Const => "'const'",
            TokenKind::Constant => "'constant'",
            TokenKind::Continue => "'continue'",
            TokenKind::Data => "'data'",
            TokenKind::Default => "'default'",
            TokenKind::Defined => "'defined'",
            TokenKind::Delete => "'delete'",
            TokenKind::Device => "'device'",
            TokenKind::Do => "'do'",
            TokenKind::Double => "'double'",
            TokenKind::DML => "'dml'",
            TokenKind::Each => "'each'",
            TokenKind::Else => "'else'",
            TokenKind::Error => "'error'",
            TokenKind::Export => "'export'",
            TokenKind::Extern => "'extern'",
            TokenKind::Event => "'event'",
            TokenKind::Field => "'field'",
            TokenKind::Float => "'float'",
            TokenKind::Footer => "'footer'",
            TokenKind::For => "'for'",
            TokenKind::Foreach => "'foreach'",
            TokenKind::Group => "'group'",
            TokenKind::Header => "'header'",
            TokenKind::Hook => "'hook'",
            TokenKind::Inline => "'inline'",
            TokenKind::Implement => "'implement'",
            TokenKind::Import => "'import'",
            TokenKind::Independent => "'independent'",
            TokenKind::If => "'if'",
            TokenKind::In => "'in'",
            TokenKind::Int => "'int'",
            TokenKind::Is => "'is'",
            TokenKind::Interface => "'interface'",
            TokenKind::Layout => "'layout'",
            TokenKind::Loggroup => "'loggroup'",
            TokenKind::Local => "'local'",
            TokenKind::Log => "'log'",
            TokenKind::Long => "'long'",
            TokenKind::Memoized => "'memoized'",
            TokenKind::Method => "'method'",
            TokenKind::New => "'new'",
            TokenKind::Param => "'param'",
            TokenKind::Port => "'port'",
            TokenKind::Provisional => "'provisional'",
            TokenKind::Register => "'register'",
            TokenKind::Return => "'return'",
            TokenKind::Saved => "'saved'",
            TokenKind::Select => "'select'",
            TokenKind::Session => "'session'",
            TokenKind::Sequence => "'sequence'",
            TokenKind::Shared => "'shared'",
            TokenKind::Short => "'short'",
            TokenKind::Signed => "'signed'",
            TokenKind::Size => "'size'",
            TokenKind::SizeOf => "'sizeof'",
            TokenKind::SizeOfType => "'sizeoftype'",
            TokenKind::Static => "'static'",
            TokenKind::Startup => "'startup'",
            TokenKind::Stringify => "'stringify'",
            TokenKind::Struct => "'struct'",
            TokenKind::Subdevice => "'subdevice'",
            TokenKind::Switch => "'switch'",
            TokenKind::Template => "'template'",
            TokenKind::Then => "'then'",
            TokenKind::This => "'this'",
            TokenKind::Throw => "'throw'",
            TokenKind::Throws => "'throws'",
            TokenKind::Try => "'try'",
            TokenKind::Typedef => "'typedef'",
            TokenKind::TypeOf => "'typeof'",
            TokenKind::Undefined => "'undefined'",
            TokenKind::Unsigned => "'unsigned'",
            TokenKind::Vect => "'vect'",
            TokenKind::Void => "'void'",
            TokenKind::Where => "'where'",
            TokenKind::While => "'while'",
            TokenKind::With => "'with'",
            TokenKind::Identifier => "identifier",
            TokenKind::IntConstant => "integer constant",
            TokenKind::FloatConstant => "float constant",
            TokenKind::HexConstant => "hexadecimal constant",
            TokenKind::BinaryConstant => "binary constant",
            TokenKind::StringConstant => "constant string",
            TokenKind::CharConstant => "constant char",
            TokenKind::Newline => "newline",
            TokenKind::Whitespace => "whitespace",
            TokenKind::Comment => "comment",
            TokenKind::MultilineComment => "comment",
            TokenKind::LexerError => "Unrecognized Token"
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use TokenKind::*;

    macro_rules! check_lexer_stream {
        ($input: expr, $ ( $e: expr),*) => {
            let mut lexer = TokenKind::lexer($input);
            $(
                {
                    let next = lexer.next();
                    assert_eq!(next, Some(Ok($e)),
                               "Got {:?}, expected {:?}", next, $e);
                }
            )*
            assert_eq!(lexer.next(), None);
        };
    }

    #[test]
    fn simple_sanity() {
        check_lexer_stream!(
            "5+5=10",
            IntConstant, Plus, IntConstant, Assign, IntConstant);
    }

    #[test]
    fn int_token_variants() {
        check_lexer_stream!("0xF00_420 0b0011100 987654321",
                            HexConstant, Whitespace, BinaryConstant,
                            Whitespace, IntConstant);
    }

    #[test]
    fn string_constants() {
        check_lexer_stream!(
            "\"string\" \"another\"",
            StringConstant, Whitespace, StringConstant);
    }

    #[test]
    fn whitespace_sanity() {
        check_lexer_stream!(
            "method foo()   {\n\treturn 5;\n}",
            Method, Whitespace, Identifier, LParen, RParen, Whitespace,
            LBrace, Newline, Whitespace, Return, Whitespace, IntConstant,
            SemiColon, Newline, RBrace);
    }

    #[test]
    fn multiline_comment() {
        check_lexer_stream!("/* ** **/", MultilineComment);
        check_lexer_stream!("/* \n* / * \n */", MultilineComment);
        check_lexer_stream!("/* \n* /* \n */", MultilineComment);
        check_lexer_stream!("/* \u{1F608} */", MultilineComment);
        check_lexer_stream!("/* \n* ... \n */ dml",
                            MultilineComment, Whitespace, DML
        );
    }

    #[test]
    fn c_block() {
        check_lexer_stream!("%{ ** %}", CBlock);
        check_lexer_stream!("%{ \n* % } \n %}", CBlock);
        check_lexer_stream!("%{ \n* {% \n %}", CBlock);
        check_lexer_stream!("%{ \u{1F608} %}", CBlock);
        check_lexer_stream!("%{ \n* ... \n %} device",
                            CBlock, Whitespace, Device);
    }

    #[test]
    fn test_unended_cblock() {
        let mut lexer = TokenKind::lexer("%{ \n");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::LexerError)));
        let mut lexer = TokenKind::lexer("%{ \n%");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::LexerError)));
    }
    #[test]
    fn test_unended_multiline_comment() {
        let mut lexer = TokenKind::lexer("/* \n");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::LexerError)));
        let mut lexer = TokenKind::lexer("/* \n*");
        assert_eq!(lexer.next(), Some(Ok(TokenKind::LexerError)));
    }

    #[test]
    fn reserved_sanity() {
        check_lexer_stream!(
            "char double float int long short signed unsigned void register",
            Char, Whitespace, Double, Whitespace, Float, Whitespace, Int,
            Whitespace, Long, Whitespace, Short, Whitespace, Signed, Whitespace,
            Unsigned, Whitespace, Void, Whitespace, Register
        );
    }
}
