% ============================================================================
% PREDICADOS AUXILIARES DE VISUALIZAÇÃO
% ============================================================================

/*
    Recebe uma lista de pares [Sigla-Nome] e usa
    recursão para imprimir a lista formatada.
 */
imprimir_lista_simples([]).
imprimir_lista_simples([Sigla-Nome|T]) :-
    format('   [~w] ~w~n', [Sigla, Nome]),
    imprimir_lista_simples(T)
.

/*
    Imprime arquivos numerados: [1] arquivo1.pl, [2] ...
    Usa recursão incrementando o IndexAtual.
 */
imprimir_arquivos_indexados([], _).
imprimir_arquivos_indexados([Arq|Resto], Index) :-
    format('   [~w] ~w~n', [Index, Arq]), 
    N is Index+1, 
    imprimir_arquivos_indexados(Resto, N)
.

/*
    Utiliza recursão para imprimir o estado ([OK] ou [ ])
    seguido do ID e da Descrição do tópico.
 */
imprimir_topicos_com_descricao([]).
imprimir_topicos_com_descricao([ID-Desc|Resto]) :-
    (sabe(ID) -> Status = '[OK]' ; Status = '[  ]'),
    format('   ~w ~w~n      Foco: ~w~n', [Status, ID, Desc]),
    imprimir_topicos_com_descricao(Resto)
.

/*
    Dado uma lista de IDs (sem descrição),
    busca a descrição no banco e imprime.
 */
listar_itens_com_descricao(_, []).
listar_itens_com_descricao(D, [ID|Resto]) :-
    topico_info(D, ID, Desc),   % Busca a descrição
    format('   - [ ] ~w~n         (~w)~n', [ID, Desc]),
    listar_itens_com_descricao(D, Resto)
.

/*
    Para cada ID, descobre de qual disciplina ele veio e sua descrição.
    Essencial para o aluno saber DE ONDE vem o pré-requisito que falta.
 */
listar_pendencias_detalhado([]).
listar_pendencias_detalhado([ID|Resto]) :-
    topico(DiscOrigem, ID),                 % Acha a disciplina dona do tópico.
    disciplina(DiscOrigem, NomeDisc, _),    % Pega o nome da disciplina.
    topico_info(DiscOrigem, ID, Desc),      % Pega a descrição do tópico.
    format('   [!] ~w (De: \'~w\')~n       Foco: ~w~n', [ID, NomeDisc, Desc]),
    listar_pendencias_detalhado(Resto)
.

/*
    Limpa a tela (imprimindo N linhas em branco)
 */
clear(N) :- forall(between(1, N, _), nl).