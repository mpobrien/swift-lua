
// TODO boolean infix and prefix operators
// TODO local
// TODO for loops
// TODO list and map literals
// a, b, c  = 3, 2, 1

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

typealias BlockStatements = Array<Statement>

func blockToStrings(block : BlockStatements) -> String {
    var out = ""
    for c in block {
        out += "\t" + c.toString()
    }
    return out
}

public struct FunctionLiteral: Expression {
    let identifier: IdentifierToken
    let params: Array<IdentifierToken>
    let body: BlockStatements
    func toString() -> String {
        let paramsStr = self.params.map { (t) -> String in t.toString() }
        var out = "function " + self.identifier.toString() + "(" +
            paramsStr.joined(separator:",") + ")\n";
        for stmt in self.body {
            out += "\t" + stmt.toString()
        }
        out += "\nend"
        return out
    }
}

public struct IntegerLiteral : Expression {
    var value: Int
    func toString() -> String {
        return String(value)
    }
}

public struct StringLiteral : Expression {
    var value: String
    func toString() -> String {
        return "'" + self.value + "'"
    }
}

public struct BooleanLiteral: Expression {
    var value: Bool
    func toString() -> String {
        if self.value {
            return "true"
        }
        return "false"
    }
}

public struct Return : Statement {
    var value : Expression
    func toString() -> String {
        return "return " + self.value.toString()
    }
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
    case IN
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
        case .IN: return "IN"
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
    override func toString() -> String {
        return "String: '\(self.value)'"
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

public class IdentifierToken : Token, Expression {
    let identifier : String
    init(identifier: String){
        self.identifier = identifier
        super.init(tokenType: TokenType.IDENT)
    }
    override func toString() -> String {
        return self.identifier
    }
}

protocol Statement {
    func toString() -> String
}

protocol Expression {
    func toString() -> String
}

struct PrefixExpression : Expression {
    var Operator: String
    var rhs : Expression
    func toString() -> String {
        return self.Operator + self.rhs.toString()
    }
}

struct InfixExpression : Expression {
    var lhs: Expression
    var Operator: String
    var rhs: Expression
    func toString() -> String {
        return self.lhs.toString() + " " + self.Operator + " " + self.rhs.toString()
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

var keywords : Dictionary<String, TokenType> = [
    "true": TokenType.TRUE,
    "in": TokenType.IN,
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

public class LoopRange : Statement {
    func toString() -> String{
        return "not implemented"
    }
}
public class StepLoopRange : LoopRange {
    let identifier : IdentifierToken
    let lowerBound : Expression
    let upperBound : Expression
    let step : Expression?
    init(
        identifier: IdentifierToken,
        lowerBound: Expression,
        upperBound: Expression,
        step: Expression?
        ){
        self.identifier = identifier
        self.lowerBound = lowerBound
        self.upperBound = upperBound
        self.step = step
    }
    override func toString() -> String {
        var out = self.identifier.toString() + " = "
        out += self.lowerBound.toString() + ", " + self.upperBound.toString()
        guard self.step != nil else {
            return out
        }
        return out + ", " + self.step!.toString()
    }
}

public class IteratorLoopRange : LoopRange {
    let identifiers : Array<IdentifierToken>
    let expressions: Array<Expression>
    init(
        identifiers: Array<IdentifierToken>,
        expressions: Array<Expression>
        ){
        self.identifiers = identifiers
        self.expressions = expressions
    }
}

public class ForStatement : Statement {
    let range: LoopRange
    let body: BlockStatements
    init(range: LoopRange, body:BlockStatements){
        self.range = range
        self.body = body
    }
    func toString() -> String {
        var out = "for " + self.range.toString() + " do\n"
        for stmt in body {
            out += "\t" + stmt.toString() + "\n"
        }
        out += "end"
        return out
    }
}

public class IfStatement : ExpressionStatement {
    var tokenType : TokenType
    var clause : Expression
    var consequence: BlockStatements
    var alternative : IfStatement? = nil
    init(
        tokenType: TokenType,
        clause : Expression,
        consequence: BlockStatements,
        alternative: IfStatement?
        ){
        self.tokenType = tokenType
        self.clause = clause
        self.consequence = consequence
        self.alternative = alternative
    }
    override func toString() -> String {
        var out = ""
        switch self.tokenType {
        case TokenType.IF:
            out = "if " + self.clause.toString() + " then\n"
        case TokenType.ELSEIF:
            out = "elseif " + self.clause.toString() + " then\n"
        case TokenType.ELSE:
            out = "else\n"
        default:
            return "?"
        }
        out += blockToStrings(block:self.consequence)
        if self.alternative == nil {
            return out + "\nend"
        }
        out += "\n" + self.alternative!.toString()
        return out
    }
}

public struct AssignStatement : Statement{
    var ident : IdentifierToken
    var rhs : Expression
    func toString() -> String  {
        return self.ident.toString() + " = " + self.rhs.toString()
    }
}

public class ExpressionStatement : Statement, Expression {
    func toString() -> String{
        return "not implemented"
    }
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
    
    func parseFuncParams() throws -> Array<IdentifierToken> {
        var out = Array<IdentifierToken>()
        while self.curToken.tokenType != TokenType.RPAREN {
            guard self.curToken.tokenType == TokenType.IDENT else {
                throw ParseError(message:"expected param name")
            }
            let ident = self.curToken as! IdentifierToken
            out.append(ident)
            self.nextToken()
            if self.curToken.tokenType == TokenType.COMMA {
                self.nextToken()
            }
        }
        return out
        
    }
    
    func parseFuncLiteral() throws -> FunctionLiteral {
        self.nextToken() // consume FUNCTION
        guard self.curToken.tokenType == TokenType.IDENT else {
            throw ParseError(message: "expected name of function")
        }
        let ident = self.curToken as! IdentifierToken
        self.nextToken()
        guard self.curToken.tokenType == TokenType.LPAREN else {
            throw ParseError(message: "expected '(', got" + self.curToken.toString())
        }
        self.nextToken()
        let params = try self.parseFuncParams()
        self.nextToken()
        
        return FunctionLiteral(
            identifier: ident,
            params: params,
            body: try parseBlock()
        )
    }
    
    func parsePrefix() throws -> Expression {
        switch self.curToken.tokenType {
        case TokenType.IDENT:
            return self.curToken as! IdentifierToken
        case TokenType.TRUE:
            return BooleanLiteral(value: true)
        case TokenType.FALSE:
            return BooleanLiteral(value: false)
        case TokenType.FUNCTION:
            return try parseFuncLiteral()
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
            if self.peekToken.tokenType != TokenType.RPAREN{
                throw ParseError(message:"Expected ')', got " + self.peekToken.toString())
            }
            return exp
        case TokenType.INT:
            let intToken = self.curToken as! IntegerToken
            guard let x = Int(intToken.value) else {
                throw ParseError(message:"Cannot parse " + intToken.value + " as an integer")
            }
            return IntegerLiteral(value: x)
        case TokenType.STRING:
            let strToken = self.curToken as! StringToken
            return StringLiteral(value: strToken.value)
        default:
            throw ParseError(message: "No prefix expression parse function for " + self.curToken.toString())
        }
    }
    
    func consumeExpected(expected: TokenType) throws {
        if self.curToken.tokenType != expected {
            throw ParseError(
                message:
                "expected [" + expected.toString() +
                    "] but got " + self.curToken.toString()
            )
        }
        self.nextToken()
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
        
        let assignStatement = AssignStatement(
            ident: lhs,
            rhs: try self.parseExpression(prec:Precedence.LOWEST)
        )
        return assignStatement
    }
    
    func parseElseStatement() throws -> IfStatement {
        self.nextToken() // consume ELSE
        let consequence = try parseBlock()
        return IfStatement(
            tokenType: TokenType.ELSE,
            clause: BooleanLiteral(value: true),
            consequence: consequence,
            alternative: nil
        )
    }
    
    func parseIfStatement() throws -> IfStatement {
        let tokType = self.curToken.tokenType
        self.nextToken() // consume IF
        let clause = try self.parseExpression(prec:Precedence.LOWEST)
        self.nextToken()
        guard self.curToken.tokenType == TokenType.THEN else {
            throw ParseError(message:"expected 'then', got" + self.curToken.toString())
        }
        self.nextToken() // consume THEN
        var consequence = BlockStatements()
        var alt : IfStatement? = nil
        blockLoop:
            while true {
                switch self.curToken.tokenType {
                case TokenType.END:
                    alt = nil
                    break blockLoop
                case TokenType.ELSEIF:
                    alt = try parseIfStatement()
                    break blockLoop
                case TokenType.ELSE:
                    alt = try parseElseStatement()
                    break blockLoop
                default:
                    consequence.append(try self.parseStatement())
                }
        }
        return IfStatement(
            tokenType: tokType,
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
    
    func parseReturn() throws -> Return {
        self.nextToken() // consume RETURN
        return Return(value: try self.parseExpression(prec: Precedence.LOWEST))
    }
    
    func parseIteratorRange() throws -> IteratorLoopRange {
        let identifiers = try self.parseNameList()
        try self.consumeExpected(expected:TokenType.IN)
        let exps = try self.parseExpList()
        return IteratorLoopRange(identifiers: identifiers, expressions: exps)
    }
    
    func parseExpList() throws -> Array<Expression> {
        var out = Array<Expression>()
        while true {
            out.append(try self.parseExpression(prec: Precedence.LOWEST))
            if self.curToken.tokenType != TokenType.COMMA {
                break
            }
            self.nextToken()
        }
        return out
    }
    
    func parseNameList() throws -> Array<IdentifierToken> {
        var out = Array<IdentifierToken>()
        while self.curToken.tokenType == TokenType.IDENT {
            out.append(self.curToken as! IdentifierToken)
            self.nextToken()
            if self.curToken.tokenType == TokenType.COMMA {
                self.nextToken()
            }
        }
        // TODO require out.length is >= 1?
        return out
    }
    
    func parseStepRange() throws -> StepLoopRange {
        // 'for' NAME '=' exp ',' exp (',' exp)? 'do' block 'end' |
        let ident = self.curToken as! IdentifierToken
        self.nextToken()
        try self.consumeExpected(expected: TokenType.ASSIGN)
        let lowerBound = try self.parseExpression(prec: Precedence.LOWEST)
        self.nextToken()
        try self.consumeExpected(expected: TokenType.COMMA)
        let upperBound = try self.parseExpression(prec: Precedence.LOWEST)
        self.nextToken()
        
        if self.curToken.tokenType == TokenType.DO {
            return StepLoopRange(
                identifier: ident,
                lowerBound: lowerBound,
                upperBound: upperBound,
                step: nil
            )
        }
        try self.consumeExpected(expected:TokenType.COMMA)
        return StepLoopRange(
            identifier: ident,
            lowerBound: lowerBound,
            upperBound: upperBound,
            step: try self.parseExpression(prec: Precedence.LOWEST)
        )
    }
    
    func parseForStatement() throws -> Statement {
        self.nextToken() // consume FOR
        let range : LoopRange
        print("fr statement", self.curToken.toString(), "dsgdsG", self.peekToken.toString())
        if self.curToken.tokenType == TokenType.IDENT &&
            self.peekToken.tokenType == TokenType.ASSIGN {
            range = try self.parseStepRange()
            print("got a step range.")
        }else {
            range = try self.parseIteratorRange()
        }
        try self.consumeExpected(expected:TokenType.DO)
        let body = try self.parseBlock()
        return ForStatement(range: range, body: body)
    }
    
    func parseStatement() throws -> Statement {
        let out : Statement
        while self.curToken.tokenType == TokenType.SEMICOLON {
            self.nextToken()
        }
        switch self.curToken.tokenType {
        case TokenType.IDENT:
            if self.peekToken.tokenType == TokenType.ASSIGN   {
                out = try self.parseAssignStatement(
                    lhs: self.curToken as! IdentifierToken
                )
                self.nextToken()
                break
            }
            throw ParseError(message:":( - " + self.curToken.toString() + " peek " + self.peekToken.toString())
        case TokenType.RETURN:
            out = try self.parseReturn()
        case TokenType.FUNCTION:
            let funcliteral = try self.parseFuncLiteral()
            out = AssignStatement(
                ident : funcliteral.identifier,
                rhs : funcliteral
            )
        case TokenType.IF:
            out = try self.parseIfStatement()
        //case TokenType.LOCAL:
        case TokenType.FOR:
            out = try self.parseForStatement()
            //case TokenType.REPEAT:
            //case TokenType.DO
            
        default:
            throw ParseError(message: "unexpected token: " + self.curToken.toString())
        }
        return out
    }
    
    func parseProgram() throws -> BlockStatements {
        var program = BlockStatements()
        while self.curToken.tokenType != TokenType.EOF {
            let statement = try self.parseStatement()
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
        self.skipWhitespace()
        switch self.ch {
        case "=":
            if self.peekChar() == "=" {
                self.readChar()
                return Token(tokenType:TokenType.EQ);
            }
            self.readChar()
            return Token(tokenType:TokenType.ASSIGN);
        case "~":
            if self.peekChar() == "=" {
                self.readChar()
                return Token(tokenType:TokenType.NOTEQ);
            }
            self.readChar()
            return Token(tokenType:TokenType.TILDE);
        case "!":
            self.readChar()
            return Token(tokenType:TokenType.BANG)
        case ";":
            self.readChar()
            return Token(tokenType:TokenType.SEMICOLON)
        case "(":
            self.readChar()
            return Token(tokenType:TokenType.LPAREN);
        case ")":
            self.readChar()
            return Token(tokenType:TokenType.RPAREN);
        case "{":
            self.readChar()
            return Token(tokenType:TokenType.LBRACE);
        case "}":
            self.readChar()
            return Token(tokenType:TokenType.RBRACE);
        case ",":
            self.readChar()
            return Token(tokenType:TokenType.COMMA);
        case "*":
            self.readChar()
            return Token(tokenType:TokenType.ASTERISK)
        case "/":
            self.readChar()
            return Token(tokenType:TokenType.SLASH)
        case "+":
            self.readChar()
            return Token(tokenType:TokenType.PLUS);
        case "\"":
            self.readChar()
            return StringToken(value:self.readString(terminator: "\""))
        case "'":
            self.readChar()
            return StringToken(value:self.readString(terminator: "'"))
        case "-":
            if self.peekChar() == "-" {
                self.readChar()
                self.readChar()
                return CommentToken(value:self.readComment())
            }
            self.readChar()
            return Token(tokenType:TokenType.MINUS);
        case "<":
            if self.peekChar() == "=" {
                self.readChar()
                self.readChar()
                return Token(tokenType:TokenType.LTE);
            }
            return Token(tokenType:TokenType.LT);
        case ">":
            if self.peekChar() == "=" {
                self.readChar()
                self.readChar()
                return Token(tokenType:TokenType.GTE);
            }
            self.readChar()
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
        while self.ch != Character("\u{0000}") && self.ch != terminator{
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
            self.readChar()
        }
        self.readChar()
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
        while self.ch.isAlpha(){
            out += String(self.ch)
            self.readChar()
        }
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

var input = "x = 22 + (34 * 10);" + 
    "for x = 1, 200 do\n" +
    "n = '4'\n" +
    "end\n" +
    "if z > 5 then\n" +
    "y = 6\n" +
    "elseif z>6 then\n" +
    "y = 7\n" +
    "else\n" +
    "y = 8\n" +
    "end\n" +
    "function xxx(foo, bar, baz)\n" +
    "n = 3\n" +
    "end\n" +
"return 4"

let lex2 = Lexer(input: input)
var tok : Token
repeat {
    tok = lex2.nextToken()
    print("x:", tok.toString())
} while tok.tokenType != TokenType.EOF


let lexer = Lexer(input: input)
let parser = Parser(lexer: lexer)
do {
    let program = try parser.parseProgram()
    for statement in program {
        print(statement.toString())
    }
}catch let err as ParseError{
    print("ParseError: ", err.message)
}

