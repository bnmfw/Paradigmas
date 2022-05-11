meuTabuleiro = [[0,0,9,0,0,5,6,0,8],
                [7,3,0,0,2,0,0,0,0],
                [5,2,6,8,0,0,0,4,7],
                [0,0,0,0,4,0,8,9,0],
                [0,0,9,6,0,3,5,0,0],
                [0,6,2,0,8,0,0,0,0],
                [2,6,0,0,0,7,9,5,1],
                [0,0,0,0,1,0,0,6,4],
                [1,0,8,6,0,0,2,0,0]]

epica = []

for indice, linha in enumerate(meuTabuleiro):
    epica.append([])
    for j, num in enumerate(linha):
        epica[indice].append((0, num))

print(epica)

with open("ajuda.txt", "w") as arquivo:
    arquivo.write(f"{epica}")