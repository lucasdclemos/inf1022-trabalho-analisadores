import ply.lex as lex
import ply.yacc as yacc
import re

# pip install ply

# ANALISADOR LÉXICO

tokens_reservados = ('INICIO', 'TERMINO', 'VIRGULA', 'IGUAL', 'MONITOR', 'EXECUTE', 'ENQUANTO', 'FACA', 'FIM', 'IF', 'THEN', 'ELSE')

t_INICIO = r'INICIO'
t_MONITOR = r'MONITOR'
t_EXECUTE = r'EXECUTE'
t_TERMINO = r'TERMINO'
t_ENQUANTO = r'ENQUANTO'
t_FACA = r'FACA'
t_FIM = r'FIM'
t_IF = r'IF'
t_THEN = r'THEN'
t_ELSE = r'ELSE'
t_VIRGULA = r','
t_IGUAL = r'='

def t_ID(t):
    r'[A-Z][A-Z0-9]*'
    if t.value in tokens_reservados:
        t.type = t.value
    else:
        t.type = 'ID'
    return t

def t_NUMERO(t):
    r'\d+'
    return t

t_ignore = ' \t\n'

def t_error(t):
    print("Caracter ilegal: ", t.value[0])
    t.lexer.skip(1)   

tokens = tokens_reservados + ('ID', 'NUMERO')

lexer = lex.lex()

# Função para testar o lexer
def test_lexer(data):
    lexer.input(data)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)

# ANALISADOR SINTÁTICO

def p_START(regras):
    '''
    START : programa
    '''
    regras[0] = regras[1]

def p_programa(regras):
    '''
    programa : INICIO varlist MONITOR varlist
    '''
    match = re.findall(r"[A-Z]", regras[4])
    for var in match:
        variaveis_monitoradas.append(var)

    regras[0] = f"int main(){{\n\t{regras[2]}\n\t{regras[4]}\n\treturn 0;\n}}"


def p_varlist(regras):
    '''
    varlist : ID
        | ID VIRGULA varlist
    '''
    if len(regras) == 2:
        regras[0] = f"int {regras[1]};"
    else:
        regras[0] = f"int {regras[1]};\n\t{regras[3]}"

def p_error(regras):
    print("Erro de sintaxe: ", regras)

parser = yacc.yacc()

def test_parser(data):
    result = parser.parse(data)
    print(result)

variaveis_monitoradas = []

def main():
    data = '''
    INICIO X, Y, Z, A, B, C MONITOR D, E, F
    '''

    test_parser(data)
    print(variaveis_monitoradas)


if __name__ == '__main__':
    main()