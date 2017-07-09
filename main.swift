extension Character
{
    func isAlpha() -> Bool {
		return (self >= "a" && self <= "z") || (self >= "A" && self <= "Z")
    }

    func isDigit() -> Bool {
		return (self >= "0" && self <= "9")
    }

	func isWhitespace() -> Bool {
		return (self == " " || self == "\t" || self == "\n" || self == "\r")
	}
}

enum Token {
	case EOF
	case EQ
	case LPAREN
	case RPAREN
	case UNKNOWN
    case ASSIGN
    case ASTERISK
    case BANG
    case COMMA
    case GT
	case GTE
    case IDENT(String)
	case ILLEGAL
    case INT(String)
    case LBRACE
    case LET
    case LT
	case LTE
    case MINUS
	case NOTEQ
	case NIL
    case PLUS
    case RBRACE
	case SEMICOLON
    case SLASH
	case WHILE
	case DO
	case IF
	case ELSE
	case ELSEIF
	case THEN
	case NOT
	case FOR
	case REPEAT
	case TILDE
	case UNTIL
	case FUNCTION
	case END
	case RETURN
	case LOCAL
	case STRING(String)

	func isEOF() -> Bool{
		switch self {
		case .EOF :
			return true
		default:
			return false
		}
	}
}

let EscapeSequences : Dictionary<Character, Character> = [
	"a": Character("\u{0007}"),
	"b": Character("\u{0008}"),
	"f": Character("\u{000C}"),
	"n": Character("\u{000A}"),
	"r": Character("\u{000D}"),
	"t": Character("\u{0009}"),
	"v": Character("\u{000B}"),
	"\\": Character("\\"),
	"\"": Character("\""),
	"'": Character("'"),
	"[": Character("["),
	"]": Character("]"),
]

var keywords : Dictionary<String, Token> = [
	"let": Token.LET,
	"nil": Token.NIL,
	"while": Token.WHILE,
	"do": Token.DO,
	"if": Token.IF,
	"else": Token.ELSE,
	"elseif": Token.ELSEIF,
	"then": Token.THEN,
	"not": Token.NOT,
	"for": Token.FOR,
	"repeat": Token.REPEAT,
	"until": Token.UNTIL,
	"function": Token.FUNCTION,
	"end": Token.END,
	"return": Token.RETURN,
	"local": Token.LOCAL,
]


public class Lexer {
    var input : String
    var position: Int = 0
    var readPosition: Int = 0
    var ch: Character = Character("\u{0000}")

    init(input: String){
        self.input = input
		self.readChar()
    }

	func nextToken() -> Token {
		var tok : Token
		self.skipWhitespace()
		switch self.ch {
 		case "=":
			if self.peekChar() == "=" {
				self.readChar()
				tok = Token.EQ;
			}else{
				tok = Token.ASSIGN;
			}
		case "~":
			if self.peekChar() == "=" {
				self.readChar()
				tok = Token.NOTEQ;
			}else{
				tok = Token.TILDE;
			}
		case "!":
			tok = Token.BANG;
		case "\"":
			tok = Token.STRING(self.readString(terminator: "\""))
		case "'":
			tok = Token.STRING(self.readString(terminator: "'"))
		case ";":
			tok = Token.SEMICOLON;
		case "(":
			tok = Token.LPAREN;
		case ")":
			tok = Token.RPAREN;
		case "{":
			tok = Token.LBRACE;
		case "}":
			tok = Token.RBRACE;
		case ",":
			tok = Token.COMMA;
		case "+":
			tok = Token.PLUS;
		case "<":
			if self.peekChar() == "=" {
				self.readChar()
				tok = Token.LTE;
			}else{
				tok = Token.LT;
			}
		case ">":
			if self.peekChar() == "=" {
				self.readChar()
				tok = Token.GTE;
			}else{
				tok = Token.GT;
			}
		case "+":
			tok = Token.PLUS;
		default:
			if self.ch.isAlpha(){
				let lit = readIdentifier()
				tok = keywords[lit] ?? Token.IDENT(lit)
			} else if self.ch.isDigit(){
				let lit = readNumber()
				tok = Token.INT(lit)
			}else{
				tok = Token.EOF;
			}
		}
		self.readChar()
		return tok
	}

	func readString(terminator : Character) -> String {
		var out : String = ""
		while true {
			self.readChar()
			if self.ch == Character("\u{0000}") || self.ch == terminator{
				break
			}
			if self.ch == "\\" {
				self.readChar()
				let escSeq = EscapeSequences[self.ch]
				if escSeq == nil {
					// TODO: invalid escape sequence - throw an error?
				}else{
					out += String(escSeq!)
				}
				continue
			}
			out += String(self.ch)
		}
		return out
		
	}

	func skipWhitespace(){
		while self.ch.isWhitespace(){
			self.readChar()
		}
	}

	func readIdentifier() -> String {
		var out : String = ""
		repeat {
			out += String(self.ch)
			self.readChar()
		} while self.ch.isAlpha()
		return out
	}

	func readNumber() -> String {
		var out : String = ""
		repeat {
			out += String(self.ch)
			self.readChar()
			// TODO: handle floating point, octal/hex, scientific notation, etc....
		} while self.ch.isDigit()
		return out
	}

    func peekChar() -> Character {
        if self.readPosition >= self.input.characters.count {
			return Character("\u{0000}")
        }
		let start = self.input.startIndex
		let end = self.input.index(start, offsetBy: self.readPosition)
		return self.input.characters[end]
	}

    func readChar() {
        if self.readPosition >= self.input.characters.count {
            self.ch = Character("\u{0000}")
        }else{
			let start = self.input.startIndex
			let end = self.input.index(start, offsetBy: self.readPosition)
            self.ch = self.input.characters[end]
        }
        
        self.position = self.readPosition
        self.readPosition += 1
    }
}

var input = "if num > 40 then\n" + 
  "print('over 40')\n" +
"elseif s ~= 'walternate' then\n" +
"io.write('not over 40\n')\n" +
"else\n" + 
  "thisIsGlobal = 5\n" +
  "local line = io.read()\n" +
  "print('Winter is coming, ' .. line)\n" +
"end" 
let lexer = Lexer(input: input)

var tok : Token
var x : Int = 0
repeat {
	tok = lexer.nextToken()
	print("x:", tok)
	x+=1;
} while !tok.isEOF()

lexer.readChar()

