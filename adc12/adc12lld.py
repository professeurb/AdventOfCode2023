from functools import cache


@cache
def nbconf(record, groups):

    # Cas de base
    if len(groups) == 0:  # plus de # à placer
        return 1 if '#' not in record else 0
    if sum(groups) + len(groups) - 1 > len(record):  # plus assez de place pour les #
        return 0

    # Récursion
    if record[0] == '.':  # si on démarre par un .
        return nbconf(record[1:], groups)

    nb = 0
    if record[0] == '?':  # ... par un ?
        # possibilités en mettant un . à la place du ?
        nb += nbconf(record[1:], groups)

    # Possibilités avec le premier groupe de # au début
    # On veut tout le début sans . et on veut qu'à l'indice taillebloc il n'y ait pas un #
    # Alors on peut placer tout le 1er bloc au début (obligatoire si on commence ou veut commencer par un #)
    if '.' not in record[:groups[0]] and (len(record) <= groups[0] or len(record) > groups[0] and record[groups[0]] != '#'):
        nb += nbconf(record[groups[0]+1:], groups[1:])

    return nb


somme = 0
with open('input12.txt', 'r') as f:
    for line in f.read().splitlines():
        record, groups = line.split(' ')
        groups = [int(x) for x in groups.split(',')]
        # tuples pour pouvoir hasher...
        somme += nbconf(tuple(record), tuple(groups))

print('Part 1 :', somme)

# Part 2
somme = 0
with open('input12.txt', 'r') as f:
    for line in f.read().splitlines():
        record, groups = line.split(' ')
        record = (5*(record + '?'))[:-1]  # attention, pas de ? à la fin
        groups = 5*[int(x) for x in groups.split(',')]
        somme += nbconf(tuple(record), tuple(groups))

print('Part 2 :', somme)  # 300ms on a crappy i5 :)
