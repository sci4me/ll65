#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub line: u32,
    pub column: u32,
    pub raw: String,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(line: u32, column: u32, raw: String, kind: TokenKind) -> Self {
        Self {
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
    source: Vec<char>,
    start: usize,
    curr: usize,
    next: Option<Token>,
    rescan: bool,
    line: u32,
    column: u32,
}

impl Lexer {
    pub fn new(source: String) -> Result<Self, String> {
        let mut result = Self {
            source: source.chars().collect::<Vec<char>>(),
            start: 0,
            curr: 0,
            next: None,
            rescan: false,
            line: 1,
            column: 1,
        };
        result.find_next_token()?;
        Ok(result)
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
                    _ => return Err(format!("Unexpected directive: {}", curr)),
                }
            }
            '>' => self.emit(TokenKind::HighByte),
            '<' => self.emit(TokenKind::LowByte),
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
                } else {
                    result = TokenKind::Char(self.next());
                }

                if self.accept("'") {
                    self.emit(result);
                } else {
                    return Err(String::from("Unclosed character"));
                }
            },
            '"' => {
                unimplemented!();
            },
            ';' => {
                while self.more() && self.next() != '\n' {}
                self.line += 1;
                self.column = 1;
                self.ignore();
                self.rescan = true;
            },
            _ => {
                if c == '0' && self.more() && self.peek() == 'x' {
                    self.next();
                    self.accept_run("0123456789");

                    let s = self.current();

                    if s.len() == 2 {
                        return Err(String::from("Empty hex literal"));
                    }

                    self.emit(TokenKind::Int(s[2..].to_string()));
                } else if c == '0' && self.more() && self.peek() == 'b' {
                    self.next();
                    self.accept_run("01");

                    let s = self.current();

                    if s.len() == 2 {
                        return Err(String::from("Empty hex literal"));
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

    pub fn get_token(&mut self) -> Token {
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_new_works() {
        assert_eq!(Token::new(42, 24, "blah".to_string(), TokenKind::Int("42".to_string())), Token {
            line: 42,
            column: 24,
            raw: "blah".to_string(),
            kind: TokenKind::Int("42".to_string())
        });
    }
}