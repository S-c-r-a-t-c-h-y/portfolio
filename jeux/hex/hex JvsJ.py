from tkinter import *
import tkinter
import tkinter.messagebox
from math import sqrt

dx = 35.6 # distance entre le centre x d'une case et le centre x de la case d'à côté
dy = 30.8 # distance entre le centre y d'une case et le centre y de la case d'en dessous
dxr = 18 # distance entre le centre x d'une case et le centre x de la case même case une ligne en dessous
x0 = 30 # coordonnée x du centre de la case en haut à gauche
y0 = 48 # coordonnée y du centre de la case en haut à gauche

taille_pion = 20

milieux = [[(x0 + dx * j + dxr * i, y0 + dy * i) for j in range(11)] for i in range(11)] # calcul des positions du centre de chaque case

tour = 0
table = [[0 for _ in range(11)] for _ in range(11)]

score_j1, score_j2 = 0, 0

# ------------------------------------------------------------------------------------------------------------------

def voisins(i: int, j: int, returnmode='valeurs', table=table) -> list:
    """
    Cette fonction retourne une liste contenant les valeurs ou les coordonnées
    des cases voisines selon l'emplacement de la case voulue et le type de retour
    
    :param i: de type int
    :param i: compris entre 0 et 10
    :param j: de type int
    :param j: compris entre 0 et 10
    :param returnmode: de type str
    :param returnmode: vaut 'valeurs' ou 'coords'
    :return: de type list
    """
    
    assert type(i) == int, "Le 1er paramètre doit être un entier."
    assert 0 <= i <= 10, "Le 1er paramètre doit être compris entre 0 et 10."
    assert type(j) == int, "Le 2eme paramètre doit être un entier."
    assert 0 <= j <= 10, "Le 2eme paramètre doit être compris entre 0 et 10."
    assert returnmode == 'valeurs' or returnmode == 'coords', "Le 3eme paramètre doit être égal à 'valeurs' ou 'coords'."
    
    # on créé une liste contenant les coordonnées théoriques des cases voisines
    _voisins = [(i-1, j), (i-1, j+1), (i, j+1), (i+1, j), (i+1, j-1), (i, j-1)]
    voisins = _voisins.copy()
    
    for couple in _voisins:
        # on vérifie qu'aucune de ces coordonnées se trouvent en dehors de la grille
        # et si oui alors on retire le couple de coordonnées
        if couple[0] < 0 or couple[0] > 10 or couple[1] < 0 or couple[1] > 10:
            voisins.remove(couple)
            
    # enfin on renvoi soit les valeurs des cases soient leur coordonnées
    return [table[couple[0]][couple[1]] for couple in voisins] if returnmode == 'valeurs' else voisins
    
# ------------------------------------------------------------------------------------------------------------------

def verifier_gain(i: int, j: int, cases_passees, table) -> bool:
    """
    Cet algortihme récursif détermine si une des deux extrémitées de la grille se relient
    et renvoi une valeur booléenne
    
    :param cases_passees: de type list
    :param i: de type int
    :param i: compris entre 0 et 10
    :param j: de type int
    :param j: compris entre 0 et 10
    :return: de type bool
    """

    assert type(i) == int, "Le 1er paramètre doit être un entier."
    assert 0 <= i <= 10, "Le 1er paramètre doit être compris entre 0 et 10."
    assert type(j) == int, "Le 2eme paramètre doit être un entier."
    assert 0 <= j <= 10, "Le 2eme paramètre doit être compris entre 0 et 10."
    assert type(cases_passees) == list, "Le 3eme paramètre doit être une liste."
    
    # comme la vérification commence toujours au début de chaque ligne
    # ou de chaque colonne, si le récursivité arrive jusqu'à l'autre bout
    # de la grille c'est forcément qu'une ligne relient les deux bouts.
    if tour % 2:
        if j == 10: return True
    else:
        if i == 10: return True
    
    # on récupère les coordonnées des cases voisines    
    v_coords = voisins(i, j, returnmode='coords', table=table)
    
    # on ajoute la case actuelle en tant que case passée
    cases_passees.append((i,j))

    # on entame le procédé de vérification
    res = []
    for case in v_coords:
        # pour toutes les case voisines, on vérifie que la case n'est pas une case
        # sur laquelle on n'est pas déjà passé puis on regarde si leur valeur
        # vaut 1 ou 2 selon le tour avant de rappeler la fonction avec les coordonnées
        # de cette case et les cases déjà passées
        if (case[0], case[1]) not in cases_passees:
            if table[case[0]][case[1]] == 1 and not tour % 2: 
                res.append(verifier_gain(case[0], case[1], cases_passees, table))
            elif table[case[0]][case[1]] == 2 and tour % 2:
                res.append(verifier_gain(case[0], case[1], cases_passees, table))
    # si un des résultats est vrai, alors une ligne relient les deux extrémités et le joueur a gagné
    return True in res
    
# ------------------------------------------------------------------------------------------------------------------
    

def distance(xa, ya, xb, yb) -> float:
    """ renvoi la distance entre les points de coordonnées (xa;ya) et (xb;yb)
    :param xa: de type int ou float
    :param ya: de type int ou float
    :param xb: de type int ou float
    :param yb: de type int ou float
    """
    return sqrt((xb-xa)**2 + (yb-ya)**2)

# ------------------------------------------------------------------------------------------------------------------

def on_click(event):
    """
        Fonction qui s'execute à chaque fois que la souris est préssée.
        Sert à lancer la vérification de la grille e l'affichage des pions.
        
        :param event: contient les coordonnées de la souris
    """
    
    global tour, score_j1, score_j2
    for i in range(11):
        for j in range(11):
            # détecte si l'emplacement de la souris au moment du click
            # se trouve à proximité d'un centre d'hexagone et ainsi dans un hexagone
            if distance(milieux[i][j][0], milieux[i][j][1], event.x, event.y) <= 15:

                if table[i][j] != 0: return # si l'hexagone est déjà utilisé on sort de la fonction

                if tour % 2: # si c'est le tour du joueur 2
                    table[i][j] = 2
                    img = Label(fenetre, image=img_bleu)
                    img.place(x=milieux[i][j][0]-11, y=milieux[i][j][1]-11) # on place l'image dans l'hexagone

                    # lance une vérificaation pour chaque ligne commençant par un 2
                    res = []
                    for k in range(11):
                        if table[k][0] == 2:
                            res.append(verifier_gain(k, 0, [], table))

                else: # si c'est le tour du joueur 1
                    table[i][j] = 1
                    img = Label(fenetre, image=img_rouge)
                    img.place(x=milieux[i][j][0]-11, y=milieux[i][j][1]-11) # on place l'image dans l'hexagone

                    # lance une vérificaation pour chaque ligne commençant par un 2
                    res = []
                    for k in range(11):
                        if table[0][k] == 1:
                            res.append(verifier_gain(0, k, [], table))

                # si une des vérification est vrai, alors un chemin est complet et le joueur a gagné
                if True in res:
                    if tour % 2:
                        tkinter.messagebox.showinfo(title='Gagné !', message=f'{nom_j2.get()} a gagné !')
                        score_j2 += 1
                        reinitialiser()
                    else:
                        tkinter.messagebox.showinfo(title='Gagné !', message=f'{nom_j1.get()} a gagné !')
                        score_j1 += 1
                        reinitialiser()
                        
                # Il y a 120 cases sur la grille donc si 120 tours sont passés et que
                # personne n'a encore gagné il y a forcément égalité
                elif tour == 120:
                    tkinter.messagebox.showinfo(title='Égalité !', message=f"Aucun joueur n'a gagné !")
                    reinitialiser()
                    
                tour += 1

# ------------------------------------------------------------------------------------------------------------------

def reinitialiser():
    """
    Fonction qui réinitialise la fenêtre de jeu en detruisant la fenêtre précedente
    et en en recréant une autre.
    """
    
    global tour, table
    fenetre.destroy() # on détruit la fenêtre actuelle
    tour = 0
    table = [[0 for _ in range(11)] for _ in range(11)]
    main() # on rappel la fonction 'main' afin de recréer une fenêtre

# ------------------------------------------------------------------------------------------------------------------

def main():
    
    # initialisation de la fenêtre
    global fenetre
    fenetre = Tk()
    fenetre.title("Jeu du hex")
    fenetre.geometry('600x500')
    fenetre.configure(bg='white')
    fenetre.resizable(width=False, height=False)

    # placement de l'image en fond
    filename = PhotoImage(file = "hex_image.png")
    background_label = Label(fenetre, image=filename)
    background_label.place(x=0, y=0)
    
    
    etiquette1 = Label(fenetre, text="Joueur 1:", font='Times 20 bold', bg='red', fg='white', height=1, width=8)
    etiquette1.place(x=0, y=410)

    etiquette2 = Label(fenetre, text="Joueur 2:", font='Times 20 bold', bg='blue', fg='white', height=1, width=8)
    etiquette2.place(x=0, y=460)

    # Création de variables de type modifiable et géré par tkinter
    global nom_j1, nom_j2, score
    nom_j1 = StringVar()
    nom_j2 = StringVar()
    score = StringVar()
    score.set(f'score : {score_j1} - {score_j2}')

    # Création des champs de saisie de texte
    saisie1 = Entry(fenetre, textvariable=nom_j1, bd=5)
    saisie1.place(x=140, y=415)
    saisie2 = Entry(fenetre, textvariable=nom_j2, bd=5)
    saisie2.place(x=140, y=465)
    
    # étiquette de score
    etiquette3 = Label(fenetre, textvariable=score, font='Times 20 bold', bg='white', fg='black', height=1, width=8)
    etiquette3.place(x=360, y=435)

    # initialisation de l'image rouge
    global img_rouge
    img_rouge =  PhotoImage(file="rouge.png")

    # initialisation de l'image bleu
    global img_bleu
    img_bleu =  PhotoImage(file="bleu.png")

    # linkage du bouton gauche de la souris avec la fonction 'on_click'
    fenetre.bind("<Button-1>", on_click)
    fenetre.mainloop()

# ------------------------------------------------------------------------------------------------------------------

if __name__ == '__main__':
    main()