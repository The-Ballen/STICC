% ============================================================================
% REGRAS DE LÓGICA (MOTOR DE INFERÊNCIA)
% ============================================================================

/*
    Esta é uma REGRA DE ADAPTAÇÃO (Wrapper).
    O arquivo 'data.pl' usa uma estrutura complexa de lista com pares (tuplas)
    ID-Descricao para economizar digitação. Porém, o motor lógico prefere fatos simples.
    Como funciona:
        1. Busca a lista completa de tópicos da disciplina (predicado lista_topicos/2);
        2. Usa 'member' para iterar item a item dessa lista;
        3. O padrão 'ID-_Descricao' ignora a descrição (usando o underline _) e extrai só o ID.
 */
topico(Disciplina, ID) :-
    lista_topicos(Disciplina, Lista),
    member(ID-_Descricao, Lista)
.

/*
    Semelhante à regra acima, mas esta extrai TAMBÉM a descrição.
    Usada quando queremos exibir detalhadamente os tópicos.
 */
topico_info(Disciplina, ID, Descricao) :-
    lista_topicos(Disciplina, Lista),
    member(ID-Descricao, Lista)
.

/*
    Verifica se a disciplina foi COMPLETAMENTE dominada.
    Lógica: "O usuário domina a Disciplina se para todo (forall) tópico T existente
        para essa Disciplina, existir um fato correspondente sabe(T) na memória."
 */
domina(Disciplina) :-
    forall(topico(Disciplina, T), sabe(T))
.

/*
    Identifica o que falta dentro da própria matéria.
    Usa 'findall' para coletar todos os casos de sucesso em uma lista.
    A condição '\+ sabe(T)' significa "NÃO PROVÁVEL que sabe T" (Negação por Falha).
 */
topicos_faltantes(Disciplina, Lista) :-
    findall(T, (
        topico(Disciplina, T),  % É um tópico da matéria...
        \+ sabe(T)              % ...e o aluno NÃO sabe.
    ), Lista)
.

/*
    Cruza informações entre disciplinas diferentes.
    Verifica os PRÉ-REQUISITOS (conteúdos anteriores) que não foram cumpridos.
 */
verificar_defasagem(Disciplina, Pendencias) :-
    findall(T, (
            requer(Disciplina, T), % A disciplina exige o tópico T...
            \+ sabe(T)             % ...e o aluno NÃO sabe T.
        ), Pendencias
    )
.