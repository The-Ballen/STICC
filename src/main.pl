/**
 * ============================================================================
 * STICC - Sistema Tutor Inteligente para Ciência da Computação
 * ============================================================================
 *
 * ESTRUTURA DO PROJETO:
 * Configurações Iniciais: Diretivas para o compilador Prolog.
 * Motor de Inferência: As "regras do jogo" (lógica pura).
 * Sistema de Persistência: Leitura e escrita em arquivos.
 * Interface de Usuário: Menus, leituras de teclado e impressões na tela.
 *
 * CONCEITOS CHAVE:
 * - Predicados Dinâmicos: Fatos que mudam em tempo de execução.
 * - Backtracking: O mecanismo do Prolog de "tentar outra opção" se algo falhar.
 * - Unificação: O "casamento" de padrões (ex: topos da lista com variáveis).
 * ============================================================================
 */

% ============================================================================
% CONFIGURAÇÃO E ESTADO
% ============================================================================

/*
    A diretiva 'dynamic' avisa ao interpretador Prolog que os predicados listados
    abaixo NÃO são estáticos. Ou seja, o programa pode adicionar (assertz) ou
    remover (retract) essas cláusulas durante a execução.
    Sem isso, daria erro ao tentar registrar que o aluno aprendeu algo.
 */
:- dynamic sabe/1.              % Armazena o conhecimento do usuário. Ex: sabe(limites).
:- dynamic arquivo_atual/1.     % Armazena o diretório do arquivo da persona atual (se existir).

/*
    'ensure_loaded' funciona como um 'import' ou 'include'.
    Ele carrega todo o conteúdo de 'X.pl' para a memória RAM.
    Se 'X.pl' não estiver na mesma pasta, o programa falha aqui.
 */
:- ensure_loaded('data.pl').
:- ensure_loaded('rules.pl').
:- ensure_loaded('persistence.pl').
:- ensure_loaded('ui.pl').
:- ensure_loaded('handlers.pl').

% ============================================================================
% INTERFACE DE USUÁRIO (CLI - Command Line Interface)
% ============================================================================

/*
    Ponto de entrada (Main).
    Apenas limpa a tela e chama o loop principal (Menu).
 */
iniciar :-
    clear(50),
    write('=============================================='), nl,
    write('      STICC - Sistema Tutor Inteligente       '), nl,
    write('   Guia Acadêmico & Organizador Curricular    '), nl,
    write('=============================================='), nl,
    menu_loop
.

/*
    Implementa um laço infinito usando estrutura de falha.
    O 'repeat', cria um ponto de retorno para sempre que o
    código falhar posteriormente.
 */
menu_loop :-
    repeat,
    nl,
    write('-------- MENU PRINCIPAL --------'), nl,
    write('[1] Listar Disciplinas (Por Período)'), nl,
    write('[2] Meu Perfil'), nl,
    write('[3] Aprender (Marcar tópico/disciplina como aprendida)'), nl,
    write('[4] Check-up de Pré-requisitos'), nl,
    write('[5] Carregar Persona'), nl,
    write('[6] Guia de Estudos'), nl,
    write('[7] Criar nova persona'), nl,
    write('[0] Salvar e Sair'), nl,

    prompt1('Opção: '),
    read(Opcao),    % Lê o input do usuário (que deve terminar com um ponto final)

    clear(50),
    tratar_opcao(Opcao), % Executa a lógica da opção escolhida
    
    % Espaçamento visual antes do 'repeat' reiniciar o loop.
    clear(4),
    
    Opcao = 0,      % Condição de parada: Se Opcao for 0, a linha seguinte (Cut) é executada.
                    % Se Opcao não for 0, a linha 'Opcao = 0' falha e o Prolog faz
                    % backtracking até o 'repeat' e executa tudo novamente.

    !               % Cut (!): Quebra o 'repeat' e encerra o predicado menu_loop.
.