## Auteur : Marie Flore
## Semble donner A010760

def modif_tab(tb):
	tab=[0 for i in range(n)]
	for i in range(n):
		tab[i]=(tb[(i-1)%n]+tb[(i+1)%n])%2
	return(tab)

def convertir(t):
	accu=0
	for i in range(len(t)):
		accu=(accu+t[i])*2
	return(accu)

def est_dans (t,e):
	for i in range(len(t)):
		if t[i]==e:
			return(i)
	return(-1)
		
for n in range(1,100):
	t=[0 for i in range(n)]
	t[0]=1
##	print(t)
	li=[]
	li.append(convertir(t))
	continuer=True
	i = 0;
	while (continuer):
		i = i + 1
		t = modif_tab(t)
##		print(t)
		nb = convertir(t)
		k = est_dans(li, nb)
		if (k == -1):
			li.append(nb)
		else:
			continuer=False
	print(i - k, ', ', sep='', end='', flush=True)
	
