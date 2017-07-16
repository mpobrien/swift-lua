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
	var identifier: String
}

public struct IntegerLiteral : Expression {
	var value: Int
}

public struct StringLiteral : Expression {
	var value: String
}

public struct BooleanLiteral: Expression {
	var value: Bool
}

public struct ParseError: Error {
	let message: String
}

enum Precedence : Int, Equatable {
	case LOWEST = 0
	case EQUALS
	case LESSGREATER
	case SUM  
	case PRODUCT 
	case PREFIX  
	case CALL 
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


public enum TokenType {
	case AND
	case OR
    case EOF
    case EQ
    case LPAREN
    case RPAREN
    case UNKNOWN
    case ASSIGN
    case ASTERISK
    case BANG
    case COMMA
	case COMMENT
    case GT
    case GTE
    case IDENT
    case ILLEGAL
    case INT
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
    case STRING
	case TRUE
	case FALSE

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
		case .AND: return "AND"
		case .OR: return "OR"
		case .EOF: return "EOF"
		case .EQ: return "EQ"
		case .LPAREN: return "LPAREN"
		case .RPAREN: return "RPAREN"
		case .UNKNOWN: return "UNKNOWN"
		case .ASSIGN: return "ASSIGN"
		case .ASTERISK: return "ASTERISK"
		case .BANG: return "BANG"
		case .COMMA: return "COMMA"
		case .COMMENT: return "COMMENT"
		case .GT: return "GT"
		case .GTE: return "GTE"
		case .IDENT: return "IDENT"
		case .ILLEGAL: return "ILLEGAL"
		case .INT: return "INT"
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
		case .TRUE: return "TRUE"
		case .FALSE: return "FALSE"
		case .STRING: return "STRING"
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

public class Token {
	let tokenType: TokenType
	init(tokenType: TokenType){
		self.tokenType = tokenType
	}
	func toString() -> String {
		return self.tokenType.toString()
	}
}

public class ValueToken : Token{
	let value: String
	init(tokenType: TokenType, value: String){
		self.value = value
		super.init(tokenType:tokenType)
	}
}

public class CommentToken: ValueToken {
	init(value: String){
		super.init(tokenType:TokenType.COMMENT, value:value)
	}
}

public class StringToken : ValueToken {
	init(value: String){
		super.init(tokenType:TokenType.STRING, value:value)
	}
}

public class IntegerToken : ValueToken {
	init(value: String){
		super.init(tokenType:TokenType.INT, value:value)
	}
	override func toString() -> String {
		return "Integer: '\(self.value)'"
	}
}

public class IdentifierToken : Token {
	let identifier : String 
	init(identifier: String){
		self.identifier = identifier
		super.init(tokenType: TokenType.IDENT)
	}
	override func toString() -> String {
		return "IDENT: '\(self.identifier)'"
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

var keywords : Dictionary<String, TokenType> = [
	"true": TokenType.TRUE,
	"false": TokenType.FALSE,
    "let": TokenType.LET,
    "nil": TokenType.NIL,
	"and": TokenType.AND,
	"or" : TokenType.OR,
    "while": TokenType.WHILE,
    "do": TokenType.DO,
    "if": TokenType.IF,
    "else": TokenType.ELSE,
    "elseif": TokenType.ELSEIF,
    "then": TokenType.THEN,
    "not": TokenType.NOT,
    "for": TokenType.FOR,
    "repeat": TokenType.REPEAT,
    "until": TokenType.UNTIL,
    "function": TokenType.FUNCTION,
    "end": TokenType.END,
    "return": TokenType.RETURN,
    "local": TokenType.LOCAL,
]


typealias BlockStatements = Array<Statement>

public class IfStatement : ExpressionStatement {
	var clause : Expression
	var consequence: BlockStatements
	var alternative : IfStatement? = nil
	init(clause : Expression,
		consequence: BlockStatements,
		alternative: IfStatement?
	){
		self.clause = clause
		self.consequence = consequence
		self.alternative = alternative
	}
}

public struct AssignStatement : Statement{
	var ident : IdentifierToken
	var rhs : Expression
}

public class ExpressionStatement : Statement, Expression {
}


public class Parser {
	let lexer : Lexer
	var curToken: Token = Token(tokenType:TokenType.ILLEGAL)
	var peekToken: Token = Token(tokenType:TokenType.ILLEGAL)
	init(lexer : Lexer){
		self.lexer = lexer;
		self.nextToken()
		self.nextToken()
	}

	func nextToken() {
		self.curToken = self.peekToken
		self.peekToken = self.lexer.nextToken()
	}

	func parsePrefix() throws -> Expression {
		switch self.curToken.tokenType {
			case TokenType.IDENT:
				let ident = (self.curToken as! IdentifierToken).identifier
				return Identifier(identifier:ident)
			case TokenType.TRUE:
				return BooleanLiteral(value: true)
			case TokenType.FALSE:
				return BooleanLiteral(value: false)
			case TokenType.MINUS:
				return PrefixExpression(
					Operator: "-",
					rhs: try self.parseExpression(prec: Precedence.PREFIX)
				)
			case TokenType.BANG:
				self.nextToken()
				return PrefixExpression(
					Operator: "!",
					rhs: try self.parseExpression(prec: Precedence.PREFIX)
				)
			case TokenType.LPAREN:
				self.nextToken()
				let exp = try self.parseExpression(prec: Precedence.LOWEST)
				if case TokenType.RPAREN(_) = self.peekToken.tokenType{ 
					throw ParseError(message:"Expected ')'")
				}
				return exp
			case TokenType.INT:
				let intToken = self.curToken as! IntegerToken
				guard let x = Int(intToken.value) else {
					throw ParseError(message:"Cannot parse '\(intToken.value)' as an integer")
				}
				return IntegerLiteral(value: x)
			case TokenType.STRING:
				let strToken = self.curToken as! StringToken
				return StringLiteral(value: strToken.value)
			default:
				throw ParseError(message: "No prefix expression parse function for " + self.curToken.toString())
		}
	}

	func parseInfix(left: Expression) throws -> Expression? {
		var opcode : String
		switch self.peekToken.tokenType {
			case TokenType.PLUS:
				opcode = "+"
			case TokenType.MINUS:
				opcode = "-"
			case TokenType.ASTERISK:
				opcode = "*"
			case TokenType.SLASH:
				opcode = "/"
			case TokenType.EQ:
				opcode = "=="
			case TokenType.NOTEQ:
				opcode = "~="
			case TokenType.LT:
				opcode = "<"
			case TokenType.GT:
				opcode = ">"
			default:
				return nil
		}	
		self.nextToken()
		let prec = self.curToken.tokenType.opPrecedence()
		self.nextToken()
		let rhs = try self.parseExpression(prec:prec)
		return InfixExpression(lhs: left, Operator: opcode, rhs: rhs)
	}

	func parseExpression(prec: Precedence) throws -> Expression {
		var left = try self.parsePrefix()
		while self.peekToken.tokenType != TokenType.SEMICOLON &&
			self.peekToken.tokenType != TokenType.EOF &&
			prec.rawValue < self.peekToken.tokenType.opPrecedence().rawValue {
			let infixLeft = try self.parseInfix(left: left) 
			if infixLeft == nil {
				return left
			}else{
				left = infixLeft!
			}
		}
		return left;
	}

	func consumeSemicolon(){
		if self.peekToken.tokenType == TokenType.SEMICOLON {
			self.nextToken()
		}
	}

	func parseAssignStatement(lhs : IdentifierToken) throws -> AssignStatement {
		self.nextToken() // consume the lhs
		self.nextToken() // consume the '=' token.
		print("assign about to consume", self.curToken.toString())

		let assignStatement = AssignStatement(
			ident: lhs, 
			rhs: try self.parseExpression(prec:Precedence.LOWEST)
		)
		print("cur token is", self.curToken.toString())
		return assignStatement
	}

	func parseIfStatement() throws -> IfStatement {
		self.nextToken() // consume IF
		print("parsing expression")
		let clause = try self.parseExpression(prec:Precedence.LOWEST)
		self.nextToken()
		guard self.curToken.tokenType == TokenType.THEN else {
			throw ParseError(message:"expected 'then', got" + self.curToken.toString())
		}
		self.nextToken() // consume THEN
		var consequence = BlockStatements()
		while self.curToken.tokenType != TokenType.END {
			consequence.append(try self.parseStatement())
			self.nextToken()
		}
		guard [TokenType.ELSE, TokenType.ELSEIF].contains(self.curToken.tokenType) else {
			return IfStatement(
				clause: clause,
				consequence: consequence,
				alternative: nil
			)
		}
		let alt = try parseIfStatement()
		return IfStatement(
			clause: clause,
			consequence: consequence,
			alternative: alt
		)
	}

	func parseBlock() throws -> BlockStatements {
		var block = BlockStatements()
		while self.curToken.tokenType != TokenType.END {
			if self.curToken.tokenType == TokenType.EOF {
				throw ParseError(message:"expected 'end' for block but reached EOF")
			}
			block.append(try self.parseStatement())
		}
		return block
	}

	func parseStatement() throws -> Statement {
		let out : Statement
		while self.curToken.tokenType == TokenType.SEMICOLON {
			self.nextToken()
		}
		switch self.curToken.tokenType {
			case TokenType.IDENT:
				if TokenType.ASSIGN == self.peekToken.tokenType {
					print("parsing assign statement")
					out = try self.parseAssignStatement(
						lhs: self.curToken as! IdentifierToken
					)
					print("parsed it, next is", self.curToken.toString())
					break
				}
				throw ParseError(message:":( - " + self.curToken.toString() + " peek " + self.peekToken.toString())
			case TokenType.IF:
				out = try self.parseIfStatement()
		default:
			throw ParseError(message: "unexpected token: " + self.curToken.toString())
		}
		return out
	}

	func parseProgram() throws -> BlockStatements {
		var program = BlockStatements()
		while self.curToken.tokenType != TokenType.EOF {
			let statement = try self.parseStatement()
			print("got statement", statement)
			program.append(statement)
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
		var tok : Token? = nil
        var tokType : TokenType
        self.skipWhitespace()
		defer {
			self.readChar()
		}
        switch self.ch {
        case "=":
            if self.peekChar() == "=" {
                self.readChar()
				return Token(tokenType:TokenType.EQ);
            }
			return Token(tokenType:TokenType.ASSIGN);
        case "~":
            if self.peekChar() == "=" {
                self.readChar()
				return Token(tokenType:TokenType.NOTEQ);
            }else{
				return Token(tokenType:TokenType.TILDE);
            }
		case "!": return Token(tokenType:TokenType.BANG)
		case ";": return Token(tokenType:TokenType.SEMICOLON)
		case "(": return Token(tokenType:TokenType.LPAREN);
		case ")": return Token(tokenType:TokenType.RPAREN);
		case "{": return Token(tokenType:TokenType.LBRACE);
		case "}": return Token(tokenType:TokenType.RBRACE);
		case ",": return Token(tokenType:TokenType.COMMA);
		case "*": return Token(tokenType:TokenType.ASTERISK)
		case "/": return Token(tokenType:TokenType.SLASH)
		case "+": return Token(tokenType:TokenType.PLUS);
        case "\"":
			return StringToken(value:self.readString(terminator: "\""))
        case "'":
			return IntegerToken(value:self.readString(terminator: "\""))
        case "-":
            if self.peekChar() == "-" {
                self.readChar()
				return CommentToken(value:self.readComment())
            }
			return Token(tokenType:TokenType.MINUS);
        case "<":
            if self.peekChar() == "=" {
                self.readChar()
				return Token(tokenType:TokenType.LTE);
            }
			return Token(tokenType:TokenType.LT);
        case ">":
            if self.peekChar() == "=" {
                self.readChar()
				return Token(tokenType:TokenType.GTE);
            }
			return Token(tokenType:TokenType.GT);
        default:
            if self.ch.isAlpha(){
                let lit = readIdentifier()
				if let val = keywords[lit] {
					return Token(tokenType: val)
				}
				return IdentifierToken(identifier: lit)
            } else if self.ch.isDigit(){
                let lit = readNumber()
				return IntegerToken(value:lit)
            }else{
				return Token(tokenType: TokenType.EOF)
            }
        }
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

var input =  "x = 22 + (34 * 10);" + 
"if z > 5 then\n" +
"y = 6\n" +
"end"

let lex2 = Lexer(input: input)
var tok : Token
repeat {
    tok = lex2.nextToken()
    print("x:", tok.toString())
} while tok.tokenType != TokenType.EOF


let lexer = Lexer(input: input)
let parser = Parser(lexer: lexer)
let program = try parser.parseProgram()
print(program)

