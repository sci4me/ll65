use crate::asm::textual::ast::*;
use crate::asm::textual::lexer::*;

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { lexer }
    }

    fn more(&self) -> bool {
        self.lexer.has_token()
    }

    fn next(&mut self) -> Result<(), String> {
        self.lexer.eat_token()
    }

    fn current(&self) -> Token {
        self.lexer.get_token()
    }

    fn accept(&self, valid: &Vec<TokenKind>) -> bool {
        if !self.more() {
            return false;
        }

        let c = self.current();
        for kind in valid {
            if c.kind == *kind {
                return true;
            }
        }

        false
    }

    fn unexpected_token_error(&self, valid: &Vec<TokenKind>) -> String {
        let mut result = String::new();
        let error = format!(
            "Unexpected token: {:?}, expected: {}",
            self.current().kind,
            valid
                .iter()
                .map(|t| format!("{:?}", t))
                .collect::<Vec<String>>()
                .join(", ")
        );
        result.push_str(&format!(
            "\u{001b}[31m\u{001b}[1merror:\u{001b}[37m {}\u{001b}[0m\n",
            error
        ));
        result.push_str(&format!(
            "   \u{001b}[34;1m-->\u{001b}[0m {}\n",
            &self.lexer.file()
        ));
        let span = self.lexer.span();
        let lines = if span.start == span.end {
            vec![(self.lexer.index_to_line(span.start), span.start)]
        } else {
            (span.start..span.end)
                .into_iter()
                .map(|i| (self.lexer.index_to_line(i), i))
                .collect::<Vec<(u32, usize)>>()
        };
        let lines = {
            let mut index = 0;
            let mut last = lines[index];
            let mut result = String::new();
            while index < lines.len() {
                let curr = lines[index];
                if curr.0 != last.0 {
                    result = format!(
                        "{}\n  \u{001b}[34m;1m{} |\u{001b}[0m {}",
                        result,
                        curr.0,
                        self.lexer.get_line(curr.1)
                    );
                } else if result.is_empty() {
                    result = format!(
                        " \u{001b}[34;1m{} |\u{001b}[0m {}",
                        curr.0,
                        self.lexer.get_line(curr.1)
                    );
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

    fn expect(&self, valid: &Vec<TokenKind>) -> Result<(), String> {
        if !self.accept(&valid) {
            return Err(self.unexpected_token_error(valid));
        }
        Ok(())
    }

    fn expect_next(&mut self, valid: &Vec<TokenKind>) -> Result<(), String> {
        self.expect(valid)?;
        self.next()?;
        Ok(())
    }

    fn parse_ident(&mut self) -> Result<NodeKind, String> {
        match self.current().kind {
            TokenKind::Ident(v) => Ok(NodeKind::Ident(IdentNode {
                value: v
            })),
            _ => Err("TODO".to_string()) 
        }
    }

    fn parse_int(&mut self, max: usize) -> Result<usize, String> {
        match self.current().kind {
            TokenKind::Int(v) => {
                v.parse::<usize>().map_err(|_| "TODO".to_string())
            },
            _ => Err("TODO".to_string())
        }
    }

    fn parse_byte(&mut self) -> Result<NodeKind, String> {
        self.parse_int(255).map(|x| NodeKind::Byte(ByteNode {
            value: x as u8
        }))
    }

    fn parse_word(&mut self) -> Result<NodeKind, String> {
        self.parse_int(65535).map(|x| NodeKind::Byte(ByteNode {
            value: x as u8
        }))
    }

    fn parse_address(&mut self) -> Result<NodeKind, String> {
        let current = self.current();
        match current.kind {
            TokenKind::Int(v) => self.parse_word(),
            TokenKind::Ident(v) => self.parse_ident(),
            _ => Err("TODO".to_string())
        }
    }

    pub fn parse(&mut self) -> Result<NodeKind, String> {
        let current = self.current();
        match current.kind {
            TokenKind::Label(v) => Ok(NodeKind::Label(LabelNode { value: v })),
            TokenKind::Ident(v) => Ok(NodeKind::Ident(IdentNode { value: v })),
            TokenKind::ResetVector => {
                Ok(NodeKind::ResetVector(ResetVectorNode {
                    value: Box::new(self.parse_address()?)
                }))
            },
            TokenKind::IrqVector => {
                Ok(NodeKind::IrqVector(IrqVectorNode {
                    value: Box::new(self.parse_address()?)
                }))
            },
            TokenKind::NmiVector => {
                Ok(NodeKind::NmiVector(NmiVectorNode {
                    value: Box::new(self.parse_address()?)
                }))
            },
            TokenKind::Macro => unimplemented!(),
            TokenKind::Byte => unimplemented!(),
            TokenKind::Word => unimplemented!(),    
            _ => {
                self.expect(&vec![
                    TokenKind::ResetVector,
                    TokenKind::IrqVector,
                    TokenKind::NmiVector,
                    TokenKind::Macro,
                    TokenKind::Byte,
                    TokenKind::Word,
                    TokenKind::Adc,
                    TokenKind::And,
                    TokenKind::Asl,
                    TokenKind::Bcc,
                    TokenKind::Bcs,
                    TokenKind::Beq,
                    TokenKind::Bit,
                    TokenKind::Bmi,
                    TokenKind::Bne,
                    TokenKind::Bpl,
                    TokenKind::Brk,
                    TokenKind::Bvc,
                    TokenKind::Bvs,
                    TokenKind::Clc,
                    TokenKind::Cld,
                    TokenKind::Cli,
                    TokenKind::Clv,
                    TokenKind::Cmp,
                    TokenKind::Cpx,
                    TokenKind::Cpy,
                    TokenKind::Dec,
                    TokenKind::Dex,
                    TokenKind::Dey,
                    TokenKind::Eor,
                    TokenKind::Inc,
                    TokenKind::Inx,
                    TokenKind::Iny,
                    TokenKind::Jmp,
                    TokenKind::Jsr,
                    TokenKind::Lda,
                    TokenKind::Ldx,
                    TokenKind::Ldy,
                    TokenKind::Lsr,
                    TokenKind::Ora,
                    TokenKind::Nop,
                    TokenKind::Pha,
                    TokenKind::Php,
                    TokenKind::Pla,
                    TokenKind::Plp,
                    TokenKind::Rol,
                    TokenKind::Ror,
                    TokenKind::Rti,
                    TokenKind::Rts,
                    TokenKind::Sbc,
                    TokenKind::Sec,
                    TokenKind::Sed,
                    TokenKind::Sei,
                    TokenKind::Sta,
                    TokenKind::Stx,
                    TokenKind::Sty,
                    TokenKind::Tax,
                    TokenKind::Tay,
                    TokenKind::Tsx,
                    TokenKind::Txa,
                    TokenKind::Txs,
                    TokenKind::Tya,
                    TokenKind::Bra,
                    TokenKind::Bbr,
                    TokenKind::Bbs,
                    TokenKind::Rmb,
                    TokenKind::Smb,
                    TokenKind::Trb,
                    TokenKind::Tsb,
                    TokenKind::Stz,
                    TokenKind::Phx,
                    TokenKind::Phy,
                    TokenKind::Plx,
                    TokenKind::Ply,
                    TokenKind::Wai,
                    TokenKind::Stp,
                ])?;

                Err("TODO".to_string())
            }
        }
    }
}
