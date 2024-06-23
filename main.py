import ply.lex as lex
import ply.yacc as yacc
import re

variaveis_monitoradas = []

# ANALISADOR LÉXICO

tokens_reservados = ('INICIO', 'TERMINO', 'VIRGULA', 'IGUAL', 'MONITOR',
                     'EXECUTE', 'ENQUANTO', 'FACA', 'FIM', 'IF', 'THEN',
                     'ELSE', 'MULT', 'MAIS', 'ZERO', 'AP', 'FP', 'VEZES',
                     'EVAL', 'MENOS', 'DIVISAO')

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
t_MULT = r'\*'
t_MAIS = r'\+'
t_MENOS = r'\-'
t_DIVISAO = r'\/'
t_ZERO = r'ZERO'
t_AP = r'\('
t_FP = r'\)'
t_VEZES = r'VEZES'
t_EVAL = r'EVAL'

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
    regras[0] = f"#include <stdio.h>\n\n{regras[1]}"

def p_programa(regras):
    '''
    programa : INICIO varlist MONITOR varlist EXECUTE cmds TERMINO
    '''
    regras[0] = f"int main(){{\n\t{regras[2]}\n\t{regras[4]}\n\t{regras[6]}\n\n\treturn 0;\n}}"

def p_varlist(regras):
    '''
    varlist : ID
        | ID VIRGULA varlist
    '''
    if len(regras) == 2:
        regras[0] = f"int {regras[1]};"
    else:
        regras[0] = f"int {regras[1]};\n\t{regras[3]}"

def p_cmds(regras):
    '''
    cmds : cmd
        | cmd cmds
    '''
    if len(regras) == 2:
        regras[0] = regras[1]
    else:
        regras[0] = f"{regras[1]}\n\t{regras[2]}"


def p_cmd(regras):
    '''
    cmd : ID IGUAL NUMERO
        | ID IGUAL ID
        | ID IGUAL NUMERO MULT NUMERO
        | ID IGUAL NUMERO MULT ID
        | ID IGUAL ID MULT NUMERO
        | ID IGUAL ID MULT ID
        | ID IGUAL NUMERO MAIS NUMERO
        | ID IGUAL NUMERO MAIS ID
        | ID IGUAL ID MAIS NUMERO
        | ID IGUAL ID MAIS ID
        | ID IGUAL NUMERO MENOS NUMERO
        | ID IGUAL NUMERO MENOS ID
        | ID IGUAL ID MENOS NUMERO
        | ID IGUAL ID MENOS ID
        | ID IGUAL NUMERO DIVISAO NUMERO
        | ID IGUAL NUMERO DIVISAO ID
        | ID IGUAL ID DIVISAO NUMERO
        | ID IGUAL ID DIVISAO ID
        | ZERO AP ID FP
        | ENQUANTO ID FACA cmds FIM
        | IF ID THEN cmds FIM
        | IF ID THEN cmds ELSE cmds FIM
        | EVAL cmds VEZES ID FIM
        | EVAL cmds VEZES NUMERO FIM
    '''
    global variaveis_monitoradas
    if regras[2] == '=':
        if len(regras) == 4:
            regras[0] = f"{regras[1]} = {regras[3]};"
        else:
            if regras[4] == '*':
                regras[0] = f"{regras[1]} = {regras[3]} * {regras[5]};"
            elif regras[4] == '+':
                regras[0] = f"{regras[1]} = {regras[3]} + {regras[5]};"
            if regras[4] == '-':
                regras[0] = f"{regras[1]} = {regras[3]} - {regras[5]};"
            elif regras[4] == '/':
                regras[0] = f"{regras[1]} = {regras[3]} / {regras[5]};"
        if regras[1] in variaveis_monitoradas:
            regras[0] = regras[0] + "\n\t" + f'printf("{regras[1]}: %d\\n", {regras[1]});'
    elif regras[1] == 'ZERO':
        regras[0] = f"{regras[3]} = 0;"
        if regras[3] in variaveis_monitoradas:
            regras[0] = regras[0] + "\n\t" + f'printf("{regras[1]}: %d\\n", {regras[1]});'
    elif regras[1] == 'ENQUANTO':
        regras[0] = f"while ({regras[2]} > 0){{\n\t{regras[4]}\n\t}}"
    elif regras[1] == 'IF':
        if regras[5] == 'FIM':
            regras[0] = f"if ({regras[2]} > 0){{\n\t{regras[4]}\n\t}}"
        elif regras[5] == 'ELSE':
            regras[
                0] = f"if ({regras[2]} > 0){{\n\t{regras[4]}\n\t}}else{{\n\t{regras[6]}\n\t}}"
    elif regras[1] == 'EVAL':
        regras[0] = f"for (int i = 0; i < {regras[4]}; i++){{\n\t{regras[2]}\n\t}}"


def p_error(regras):
    print("Erro de sintaxe: ", regras)

parser = yacc.yacc()

def test_parser(data):
    result = parser.parse(data)
    return result

def main():
    global variaveis_monitoradas

    nome_arquivo_input = input("Digite o nome do arquivo .provol-one que deve ser o input do analisador (não adicione a extensão .provol-one): ")

    with open(f'{nome_arquivo_input}.provol-one') as file:
        data = file.read()
    
    match = re.search(r'MONITOR (.*)\nEXECUTE', data)
    vars = re.findall(r'[A-Z]', match.group(1))
    for item in vars:
        variaveis_monitoradas.append(item)
          
    result = test_parser(data)
    print(result)

    f = open('codigo.c', 'w')
    f.write(result)

if __name__ == '__main__':
    main()
