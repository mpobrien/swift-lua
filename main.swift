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

public struct Identifier : Expression {
	var name: String
}

public struct IntegerLiteral : Expression {
	var value: Int
}


public struct StringLiteral : Expression {
	var value: String
}

public class Stuff : Expression {
	var tok: Token
	init(tok: Token){
		self.tok = tok
	}
}

public struct ParseError: Error {
	let message: String
}

enum Precedence : Int {
	case LOWEST = 0
	case EQUALS = 1
	case LESSGREATER = 2
	case SUM  = 3
	case PRODUCT = 4
	case PREFIX  = 5
	case CALL = 6
	func toString() -> String{
		switch self {
			case .LOWEST: return "LOWEST"
			case .EQUALS: return "EQUALS"
			case .LESSGREATER: return "LESSGREATER"
			case .SUM : return "SUM"
			case .PRODUCT: return "PRODUCT"
			case .PREFIX : return "PREFIX"
			case .CALL: return "CALL"
		}
	}
}


public enum Token {
    case EOF
    case EQ
    case LPAREN
    case RPAREN
    case UNKNOWN
    case ASSIGN
    case ASTERISK
    case BANG
    case COMMA
	case COMMENT(String)
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

	func opPrecedence() -> Precedence {
		switch self {
			case .EQ: return Precedence.EQUALS
			case .NOTEQ: return Precedence.EQUALS
            case .LT: return Precedence.LESSGREATER
    		case .GT: return Precedence.LESSGREATER
			case .PLUS: return Precedence.SUM
			case .MINUS: return Precedence.SUM
			case .SLASH: return Precedence.PRODUCT
			case .ASTERISK: return Precedence.PRODUCT
		default:
			return Precedence.LOWEST
		}
	}

	func toString() -> String {
		switch self {
		case .EOF: return "EOF"
		case .EQ: return "EQ"
		case .LPAREN: return "LPAREN"
		case .RPAREN: return "RPAREN"
		case .UNKNOWN: return "UNKNOWN"
		case .ASSIGN: return "ASSIGN"
		case .ASTERISK: return "ASTERISK"
		case .BANG: return "BANG"
		case .COMMA: return "COMMA"
		case let .COMMENT(x): return "COMMENT[" + x + "]"
		case .GT: return "GT"
		case .GTE: return "GTE"
		case let .IDENT(x): return "IDENT[" + x + "]" 
		case .ILLEGAL: return "ILLEGAL"
		case let .INT(x): return "INT[" + x + "]"
		case .LBRACE: return "LBRACE"
		case .LET: return "LET"
		case .LT: return "LT"
		case .LTE: return "LTE"
		case .MINUS: return "MINUS"
		case .NOTEQ: return "NOTEQ"
		case .NIL: return "NIL"
		case .PLUS: return "PLUS"
		case .RBRACE: return "RBRACE"
		case .SEMICOLON: return "SEMICOLON"
		case .SLASH: return "SLASH"
		case .WHILE: return "WHILE"
		case .DO: return "DO"
		case .IF: return "IF"
		case .ELSE: return "ELSE"
		case .ELSEIF: return "ELSEIF"
		case .THEN: return "THEN"
		case .NOT: return "NOT"
		case .FOR: return "FOR"
		case .REPEAT: return "REPEAT"
		case .TILDE: return "TILDE"
		case .UNTIL: return "UNTIL"
		case .FUNCTION: return "FUNCTION"
		case .END: return "END"
		case .RETURN: return "RETURN"
		case .LOCAL: return "LOCAL"
		case let .STRING(x):
			return "STRING[" + x + "]"
		}
	}
    
    func isEOF() -> Bool{
        switch self {
        case .EOF :
            return true
        default:
            return false
        }
    }
}

protocol Statement {
}

protocol Expression {
}

struct PrefixExpression : Expression {
	var Operator: String
	var rhs : Expression
}

struct InfixExpression : Expression {
	var lhs: Expression
	var Operator: String
	var rhs: Expression
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


typealias Program = Array<Statement>

public struct AssignStatement : Statement{
	var ident : String
	var rhs : Expression
}

public struct ExpressionStatement : Statement {
	var expression : Expression
}


public class Parser {
	let lexer : Lexer
	var curToken: Token = Token.ILLEGAL
	var peekToken: Token = Token.ILLEGAL 
	init(lexer : Lexer){
		self.lexer = lexer;
		self.nextToken()
		self.nextToken()
	}

/*
var precedences = map[token.TokenType]int{ token.EQ: EQUALS,
token.NOT_EQ: EQUALS,
    token.LT:
    token.GT:
    token.PLUS:
    token.MINUS:    SUM,
    token.SLASH:    PRODUCT,
    token.ASTERISK: PRODUCT,
}
// [...]
func (p *Parser) peekPrecedence() int {
if p, ok := precedences[p.peekToken.Type]; ok {
return p }
return LOWEST }
*/



	/*
	func expectPeek(tokenType: Token) {
		if  case self.peekToken == tokenType {
		}
		if p.peekTokenIs(t) {
		    p.nextToken()
				   return true } else {
					   return false }
			*/
	func nextToken() {
		self.curToken = self.peekToken
		self.peekToken = self.lexer.nextToken()
	}

	func parsePrefix() throws -> Expression {
		switch self.curToken {
			case let Token.IDENT(ident):
				return Identifier(name: ident)
			case let Token.INT(integer):
				guard let x = Int(integer) else {
					throw ParseError(message:"Cannot parse '\(integer)' as an integer")
				}
				return IntegerLiteral(value: x)
			case let Token.STRING(x):
				return StringLiteral(value: x)
			default:
				throw ParseError(message: "No prefix expression parse function for " + self.curToken.toString())
		}
	}

	func parseInfix(left: Expression) throws -> Expression? {
		var opcode : String
		switch self.peekToken {
			case Token.PLUS:
				opcode = "+"
			case Token.MINUS:
				opcode = "+"
			case Token.ASTERISK:
				opcode = "+"
			case Token.SLASH:
				opcode = "+"
			case Token.EQ:
				opcode = "=="
			case Token.NOTEQ:
				opcode = "~="
			case Token.LT:
				opcode = "<"
			case Token.GT:
				opcode = ">"
			default:
				return nil
		}	
		self.nextToken()
		let prec = self.curToken.opPrecedence()
		self.nextToken()
		let rhs = try self.parseExpression(prec:prec)
		return InfixExpression(lhs: left, Operator: opcode, rhs: rhs)
	}

	func parseExpression(prec: Precedence) throws -> Expression {
		var left = try self.parsePrefix()
		while true {
			if case Token.SEMICOLON = self.peekToken {
				break;
			}
			if case Token.EOF = self.peekToken {
				break;
			}
			if prec.rawValue >= self.peekToken.opPrecedence().rawValue {
				break;
			}
			let infixLeft = try self.parseInfix(left: left) 
			if infixLeft == nil {
				return left
			}else{
				left = infixLeft!
			}
		}

		return left;
		

/*
		let left = try self.curToken.parsePrefix()
		for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
			 infix := p.infixParseFns[p.peekToken.Type]
if infix == nil {
return leftExp }
           p.nextToken()
           leftExp = infix(leftExp)
       }
*/



		switch self.curToken {
			case let Token.IDENT(ident):
				return Identifier(name: ident)
			default:
				throw ParseError(message:"no parse expression for " + self.curToken.toString());
		}
	}

	func parseAssignStatement(lhs : String) throws -> AssignStatement {
		self.nextToken() // consume the lhs
		self.nextToken() // consume the '=' token.

		return AssignStatement(
			ident: lhs, 
			rhs: try self.parseExpression(prec:Precedence.LOWEST)
		)
	}

	func parseExpressionStatement() throws -> ExpressionStatement {
		return ExpressionStatement(expression: Stuff(tok:Token.ILLEGAL))
		
	}

	func parseStatement() throws -> Statement {
		switch self.curToken {
			case let Token.IDENT(ident):
				if case Token.ASSIGN = self.peekToken {
					return try self.parseAssignStatement(lhs: ident)
				}

				throw ParseError(message:":(")
		default:
			throw ParseError(message: "unexpected token: " + self.curToken.toString())
		}
	}

	func parseProgram() throws -> Program {
		var program = Program()
		while !self.curToken.isEOF() {
			let s = try self.parseStatement()
			program.append(s)
			self.nextToken()
		}
		return program
	}

}

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
        case "-":
            if self.peekChar() == "-" {
                self.readChar()
                tok = Token.COMMENT(self.readComment())
            }else{
                tok = Token.MINUS;
            }
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

    func readComment() -> String {
        var out : String = ""
        while true {
            self.readChar()
			if self.ch == "\n" || self.ch == Character("\u{0000}") {
				break;
			}
            out += String(self.ch)
        }
        return out
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

/*
var input2 = "if num > 40 then\n" + 
    "print('over 40')\n" +
    "elseif s ~= 'walternate' then\n" +
    "io.write('not over 40\n') -- hey sup\n" +
    "else\n" + 
    "thisIsGlobal = 5\n" +
    "local line = io.read()\n" +
    "print('Winter is coming, ' .. line)\n" +
"end" 
*/

var input =  "x = 22 + 34"

let lex2 = Lexer(input: input)
var tok : Token
repeat {
    tok = lex2.nextToken()
    print("x:", tok)
} while !tok.isEOF()






let lexer = Lexer(input: input)
let parser = Parser(lexer: lexer)
let program = try parser.parseProgram()
print(program)

