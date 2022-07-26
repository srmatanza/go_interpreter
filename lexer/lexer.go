package lexer

import (
	"github.com/srmatanza/go_interpreter/token"

	"unicode"
	"unicode/utf8"
)

type Lexer struct {
	input string
	ch    rune
}

func New(input string) *Lexer {
	lexer := &Lexer{input: input}
	lexer.readChar()
	return lexer
}

func (l *Lexer) readChar() (ret rune) {
	if len(l.input) > 0 {
		r, size := utf8.DecodeRuneInString(l.input)
		ret = l.ch
		l.ch = r
		l.input = l.input[size:]
	} else {
		ret = l.ch
		l.ch = 0
	}

	return ret
}

func (l *Lexer) peekChar() (ret rune) {
	if len(l.input) > 0 {
		ret, _ = utf8.DecodeRuneInString(l.input)
	}
	return
}

func (l *Lexer) readIdentifier() (ret string) {
	for unicode.IsLetter(l.ch) {
		ret += string(l.readChar())
	}
	return
}

func (l *Lexer) readNumber() (ret string) {
	for unicode.IsNumber(l.ch) {
		ret += string(l.readChar())
	}
	return
}

func (l *Lexer) skipWhiteSpace() {
	for isWhitespace(l.ch) {
		l.readChar()
	}
}

func isWhitespace(r rune) bool {
	if r == '\n' || r == '\r' || r == ' ' || r == '\t' {
		return true
	}

	return false
}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.skipWhiteSpace()

	switch l.ch {
	case '=':
		if l.peekChar() == '=' {
			tok.Type = token.EQ
			tok.Literal = "=="
			l.readChar()
		} else {
			tok = newToken(token.ASSIGN, l.ch)
		}
	case '+':
		tok = newToken(token.PLUS, l.ch)
	case '-':
		tok = newToken(token.MINUS, l.ch)
	case '!':
		if l.peekChar() == '=' {
			tok.Type = token.NEQ
			tok.Literal = "!="
			l.readChar()
		} else {
			tok = newToken(token.BANG, l.ch)
		}
	case '/':
		tok = newToken(token.SLASH, l.ch)
	case '*':
		tok = newToken(token.ASTERISK, l.ch)
	case '<':
		if l.peekChar() == '=' {
			tok.Type = token.LTE
			tok.Literal = "<="
			l.readChar()
		} else {
			tok = newToken(token.LT, l.ch)
		}
	case '>':
		if l.peekChar() == '=' {
			tok.Type = token.GTE
			tok.Literal = ">="
			l.readChar()
		} else {
			tok = newToken(token.GT, l.ch)
		}
	case ';':
		tok = newToken(token.SEMICOLON, l.ch)
	case '(':
		tok = newToken(token.LPAREN, l.ch)
	case ')':
		tok = newToken(token.RPAREN, l.ch)
	case '{':
		tok = newToken(token.LBRACE, l.ch)
	case '}':
		tok = newToken(token.RBRACE, l.ch)
	case '[':
		tok = newToken(token.LBRACKET, l.ch)
	case ']':
		tok = newToken(token.RBRACKET, l.ch)
	case ',':
		tok = newToken(token.COMMA, l.ch)
	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if unicode.IsLetter(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal)
			return tok
		} else if unicode.IsNumber(l.ch) {
			tok.Literal = l.readNumber()
			tok.Type = token.INT
			return tok
		} else {
			tok = newToken(token.ILLEGAL, l.ch)
		}
	}

	l.readChar()
	return tok
}

func newToken(tokenType token.TokenType, ch rune) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}
