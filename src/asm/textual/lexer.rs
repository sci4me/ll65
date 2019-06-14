use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub file: String,
    pub span: Span,
    pub line: u32,
    pub column: u32,
    pub raw: String,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(file: String, span: Span, line: u32, column: u32, raw: String, kind: TokenKind) -> Self {
        Self {
            file,
            span,
            line,
            column,
            raw,
            kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    ResetVector,
    IrqVector,
    NmiVector,

    HighByte,
    LowByte,

    Macro,
    EndMacro,
    MacroParameter(String),

    Label(String),
    Ident(String),
    Int(String),
    Char(char),
    Str(String),

    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Beq,
    Bit,
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Ora,
    Nop,
    Pha,
    Php,
    Pla,
    Plp,
    Rol,
    Ror,
    Rti,
    Rts,
    Sbc,
    Sec,
    Sed,
    Sei,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
    Bra,
    Bbr,
    Bbs,
    Rmb,
    Smb,
    Trb,
    Tsb,
    Stz,
    Phx,
    Phy,
    Plx,
    Ply,
    Wai,
    Stp,
}

pub struct Lexer {
    file: String,
    source: Vec<char>,
    start: usize,
    curr: usize,
    next: Option<Token>,
    rescan: bool,
    line: u32,
    column: u32,
    line_map: HashMap<usize, u32>
}

impl Lexer {
    pub fn new(file: String, source: String) -> Result<Self, String> {
        let mut result = Self {
            file,
            source: source.chars().collect::<Vec<char>>(),
            start: 0,
            curr: 0,
            next: None,
            rescan: false,
            line: 1,
            column: 1,
            line_map: HashMap::new()
        };
        result.compute_line_map();
        result.find_next_token()?;
        Ok(result)
    }

    fn compute_line_map(&mut self) {
        let mut line = 1;
        for i in 0..self.source.len() {
            if self.source[i] == '\n' {
                line += 1;
            }
            self.line_map.insert(i, line);
        }
    }

    fn more(&self) -> bool {
        self.curr < self.source.len()
    }

    fn peek(&self) -> char {
        self.source[self.curr]
    }

    fn next(&mut self) -> char {
        let c = self.source[self.curr];
        self.curr += 1;
        self.column += 1;
        c
    }

    fn current(&self) -> String {
        self.source[self.start..self.curr]
            .iter()
            .cloned()
            .collect::<String>()
    }

    fn accept(&mut self, valid: &str) -> bool {
        if self.more() && valid.contains(self.peek()) {
            self.next();
            return true;
        }
        false
    }

    fn accept_run(&mut self, valid: &str) {
        while self.accept(valid) {}
    }

    fn ignore(&mut self) {
        self.start = self.curr;
        self.next = None;
    }

    fn emit(&mut self, token: TokenKind) {
        assert!(!self.has_token());
        let s = self.current();
        self.next = Some(Token::new(
            self.file.clone(),
            Span::new(self.start, self.curr),
            self.line,
            self.column - s.len() as u32,
            s,
            token,
        ));
        self.start = self.curr;
    }

    fn skip_whitespace(&mut self) {
        while self.more() {
            match self.peek() {
                '\n' => {
                    self.line += 1;
                    self.column = 0;
                }
                '\t' | ' ' => {}
                _ => break,
            }
            self.next();
        }
        self.ignore();
    }

    fn find_next_token(&mut self) -> Result<(), String> {
        self.skip_whitespace();

        if !self.more() {
            self.next = None;
            return Ok(());
        }

        let c = self.next();
        match c {
            '\n' => {
                self.ignore();
                self.line += 1;
                self.column = 1;
            }
            '.' => {
                self.accept_run("abcdefghijklmnopqrstuvwxyz");
                let curr = self.current();
                match curr.as_str() {
                    ".reset" => self.emit(TokenKind::ResetVector),
                    ".irq" => self.emit(TokenKind::IrqVector),
                    ".nmi" => self.emit(TokenKind::NmiVector),
                    ".macro" => self.emit(TokenKind::Macro),
                    ".endmacro" => self.emit(TokenKind::EndMacro),
                    _ => return Err(format!("Unexpected directive: {}", curr)),
                }
            }
            '>' => self.emit(TokenKind::HighByte),
            '<' => self.emit(TokenKind::LowByte),
            '$' => {
                self.accept_run("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_");
                self.accept_run("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789");
                self.emit(TokenKind::MacroParameter(self.current()[1..].to_string()));
            }
            '\'' => {
                let result;

                if self.accept("\\") {
                    if self.accept("\\") {
                        result = TokenKind::Char('\\');
                    } else if self.accept("\"") {
                        result = TokenKind::Char('\"');
                    } else if self.accept("'") {
                        result = TokenKind::Char('\'');
                    } else if self.accept("n") {
                        result = TokenKind::Char('\n');
                    } else {
                        return Err(format!("Unexpected escape character: {}", self.peek()));
                    }
                } else if self.more() {
                    result = TokenKind::Char(self.next());
                } else {
                    return Err(String::from("Unclosed character"));
                }

                if self.accept("'") {
                    self.emit(result);
                } else {
                    return Err(String::from("Unclosed character"));
                }
            }
            '"' => {
                unimplemented!();
            }
            ';' => {
                while self.more() && self.next() != '\n' {}
                self.line += 1;
                self.column = 1;
                self.ignore();
                self.rescan = true;
            }
            _ => {
                if c == '0' && self.more() && self.peek() == 'x' {
                    self.next();
                    self.accept_run("0123456789");

                    let s = self.current();

                    if s.len() == 2 {
                        return Err(String::from("Incomplete hex literal"));
                    }

                    self.emit(TokenKind::Int(s[2..].to_string()));
                } else if c == '0' && self.more() && self.peek() == 'b' {
                    self.next();
                    self.accept_run("01");

                    let s = self.current();

                    if s.len() == 2 {
                        return Err(String::from("Incomplete binary literal"));
                    }

                    self.emit(TokenKind::Int(s[2..].to_string()));
                } else if c >= '1' && c <= '9' {
                    self.accept_run("0123456789");
                    self.emit(TokenKind::Int(self.current().to_string()))
                } else if char::is_alphabetic(c) || c == '_' {
                    self.accept_run(
                        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789",
                    );

                    let curr = self.current();

                    if self.accept(":") {
                        self.emit(TokenKind::Label(curr));
                    } else {
                        match curr.as_str() {
                            "adc" => self.emit(TokenKind::Adc),
                            "and" => self.emit(TokenKind::And),
                            "asl" => self.emit(TokenKind::Asl),
                            "bcc" => self.emit(TokenKind::Bcc),
                            "bcs" => self.emit(TokenKind::Bcs),
                            "beq" => self.emit(TokenKind::Beq),
                            "bit" => self.emit(TokenKind::Bit),
                            "bmi" => self.emit(TokenKind::Bmi),
                            "bne" => self.emit(TokenKind::Bne),
                            "bpl" => self.emit(TokenKind::Bpl),
                            "brk" => self.emit(TokenKind::Brk),
                            "bvc" => self.emit(TokenKind::Bvc),
                            "bvs" => self.emit(TokenKind::Bvs),
                            "clc" => self.emit(TokenKind::Clc),
                            "cld" => self.emit(TokenKind::Cld),
                            "cli" => self.emit(TokenKind::Cli),
                            "clv" => self.emit(TokenKind::Clv),
                            "cmp" => self.emit(TokenKind::Cmp),
                            "cpx" => self.emit(TokenKind::Cpx),
                            "cpy" => self.emit(TokenKind::Cpy),
                            "dec" => self.emit(TokenKind::Dec),
                            "dex" => self.emit(TokenKind::Dex),
                            "dey" => self.emit(TokenKind::Dey),
                            "eor" => self.emit(TokenKind::Eor),
                            "inc" => self.emit(TokenKind::Inc),
                            "inx" => self.emit(TokenKind::Inx),
                            "iny" => self.emit(TokenKind::Iny),
                            "jmp" => self.emit(TokenKind::Jmp),
                            "jsr" => self.emit(TokenKind::Jsr),
                            "lda" => self.emit(TokenKind::Lda),
                            "ldx" => self.emit(TokenKind::Ldx),
                            "ldy" => self.emit(TokenKind::Ldy),
                            "lsr" => self.emit(TokenKind::Lsr),
                            "ora" => self.emit(TokenKind::Ora),
                            "nop" => self.emit(TokenKind::Nop),
                            "pha" => self.emit(TokenKind::Pha),
                            "php" => self.emit(TokenKind::Php),
                            "pla" => self.emit(TokenKind::Pla),
                            "plp" => self.emit(TokenKind::Plp),
                            "rol" => self.emit(TokenKind::Rol),
                            "ror" => self.emit(TokenKind::Ror),
                            "rti" => self.emit(TokenKind::Rti),
                            "rts" => self.emit(TokenKind::Rts),
                            "sbc" => self.emit(TokenKind::Sbc),
                            "sec" => self.emit(TokenKind::Sec),
                            "sed" => self.emit(TokenKind::Sed),
                            "sei" => self.emit(TokenKind::Sei),
                            "sta" => self.emit(TokenKind::Sta),
                            "stx" => self.emit(TokenKind::Stx),
                            "sty" => self.emit(TokenKind::Sty),
                            "tax" => self.emit(TokenKind::Tax),
                            "tay" => self.emit(TokenKind::Tay),
                            "tsx" => self.emit(TokenKind::Tsx),
                            "txa" => self.emit(TokenKind::Txa),
                            "txs" => self.emit(TokenKind::Txs),
                            "tya" => self.emit(TokenKind::Tya),
                            "bra" => self.emit(TokenKind::Bra),
                            "bbr" => self.emit(TokenKind::Bbr),
                            "bbs" => self.emit(TokenKind::Bbs),
                            "rmb" => self.emit(TokenKind::Rmb),
                            "smb" => self.emit(TokenKind::Smb),
                            "trb" => self.emit(TokenKind::Trb),
                            "tsb" => self.emit(TokenKind::Tsb),
                            "stz" => self.emit(TokenKind::Stz),
                            "phx" => self.emit(TokenKind::Phx),
                            "phy" => self.emit(TokenKind::Phy),
                            "plx" => self.emit(TokenKind::Plx),
                            "ply" => self.emit(TokenKind::Ply),
                            "wai" => self.emit(TokenKind::Wai),
                            "stp" => self.emit(TokenKind::Stp),
                            _ => self.emit(TokenKind::Ident(curr)),
                        }
                    }
                } else {
                    return Err(format!("Unexpected character: '{}'", c));
                }
            }
        }

        Ok(())
    }

    pub fn has_token(&self) -> bool {
        self.next.is_some()
    }

    pub fn get_token(&self) -> Token {
        self.next.clone().unwrap()
    }

    pub fn eat_token(&mut self) -> Result<(), String> {
        self.ignore();

        loop {
            self.rescan = false;
            self.find_next_token()?;

            if !self.rescan {
                break;
            }
        }

        Ok(())
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn column(&self) -> u32 {
        self.column
    }

    pub fn index_to_line(&self, index: usize) -> u32 {
        assert!(index < self.source.len());
        *self.line_map.get(&index).expect("Internal Error")
    }

    pub fn span(&self) -> Span {
        if self.curr == self.source.len() {
            Span::new(self.start, self.curr - 1)
        } else {
            Span::new(self.start, self.curr)
        }
    }

    pub fn get_in_span(&self, span: Span) -> String {
        self.source[span.start..span.end].iter().collect::<String>()
    }

    pub fn get_line(&self, index: usize) -> String {
        assert!(self.source[index] != '\n');

        let start = {
            let mut i = index;
            loop {
                if i == 0 || self.source[i - 1] == '\n' {
                    break i;
                }
                i -= 1;
            }
        };

        let end = {
            let mut i = index;
            loop {
                if i + 1 >= self.source.len() || self.source[i + 1] == '\n' {
                    break i;
                }
                i += 1;
            }
        };

        self.source[start..end+1].iter().collect::<String>()
    }

    pub fn get_lines_in_span(&self, span: Span) -> Vec<String> {
        assert!(span.end < self.source.len());

        let mut result = Vec::new();

        let mut last: Option<String> = None;
        for i in span.start..span.end+1 {
            if self.source[i] == '\n' {
                continue;
            }

            let curr = self.get_line(i);
            if let Some(last) = last {
                if last != curr {
                    result.push(curr.clone());
                }
            } else {
                result.push(curr.clone());
            }
            last = Some(curr);
        }

        result
    }

    pub fn format_error_message(&self, error: String) -> String {
        let mut result = String::new();
        result.push_str(&format!("\u{001b}[31m\u{001b}[1merror:\u{001b}[37m {}\u{001b}[0m\n", error));
        result.push_str(&format!("   \u{001b}[34;1m-->\u{001b}[0m {}\n", &self.file));
        let span = self.span();
        let lines = if span.start == span.end {
            vec![(self.index_to_line(span.start), span.start)]
        } else {
            (span.start..span.end)
                .into_iter()
                .map(|i| (self.index_to_line(i), i))
                .collect::<Vec<(u32, usize)>>()
        };
        let lines = {
            let mut index = 0;
            let mut last = lines[index];
            let mut result = String::new();
            while index < lines.len() {
                let curr = lines[index];
                if curr.0 != last.0 {
                    result = format!("{}\n  \u{001b}[34m;1m{} |\u{001b}[0m {}", result, curr.0, self.get_line(curr.1));
                } else if result.is_empty() {
                    result = format!(" \u{001b}[34;1m{} |\u{001b}[0m {}", curr.0, self.get_line(curr.1));
                }
                index += 1;
                last = curr;
            }
            result
        };
        result.push_str(&format!("    \u{001b}[34;1m|\u{001b}[0m\n"));
        result.push_str(&format!("{}\n", lines));
        result.push_str(&format!("    \u{001b}[34;1m|\u{001b}[0m"));
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_new_works() {
        assert_eq!(Span::new(1, 4), Span{start: 1, end: 4});
    }

    #[test]
    fn token_new_works() {
        assert_eq!(
            Token::new(
                "foo.s".to_string(), 
                Span::new(4, 8),
                42,
                24,
                "blah".to_string(),
                TokenKind::Int("42".to_string())
            ),
            Token {
                file: "foo.s".to_string(),
                span: Span { start: 4, end: 8 },
                line: 42,
                column: 24,
                raw: "blah".to_string(),
                kind: TokenKind::Int("42".to_string())
            }
        );
    }
}
