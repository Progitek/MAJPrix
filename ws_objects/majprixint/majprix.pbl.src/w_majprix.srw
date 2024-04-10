$PBExportHeader$w_majprix.srw
forward
global type w_majprix from window
end type
type cb_14 from commandbutton within w_majprix
end type
type cb_1 from commandbutton within w_majprix
end type
type cb_13 from commandbutton within w_majprix
end type
type cb_12 from commandbutton within w_majprix
end type
type cb_11 from commandbutton within w_majprix
end type
type cb_10 from commandbutton within w_majprix
end type
type cb_9 from commandbutton within w_majprix
end type
type cb_8 from commandbutton within w_majprix
end type
type cb_7 from commandbutton within w_majprix
end type
type cb_6 from commandbutton within w_majprix
end type
type cb_5 from commandbutton within w_majprix
end type
type cb_4 from commandbutton within w_majprix
end type
type cb_3 from commandbutton within w_majprix
end type
type cb_2 from commandbutton within w_majprix
end type
type cbx_indien from checkbox within w_majprix
end type
type cbx_ramq from checkbox within w_majprix
end type
type cbx_ass from checkbox within w_majprix
end type
type cbx_pat from checkbox within w_majprix
end type
type cbx_prixlab from checkbox within w_majprix
end type
type st_2 from statictext within w_majprix
end type
type cbx_duree from checkbox within w_majprix
end type
type cbx_prod from checkbox within w_majprix
end type
type cbx_nbcanaux from checkbox within w_majprix
end type
type cbx_materiel from checkbox within w_majprix
end type
type cbx_3 from checkbox within w_majprix
end type
type cbx_descfr from checkbox within w_majprix
end type
type cbx_descan from checkbox within w_majprix
end type
type ddplb_guide from dropdownpicturelistbox within w_majprix
end type
type st_guide from statictext within w_majprix
end type
type cb_quitter from commandbutton within w_majprix
end type
type cb_maj from commandbutton within w_majprix
end type
type st_soustitre from statictext within w_majprix
end type
type dw_listeguide from datawindow within w_majprix
end type
type st_titre from statictext within w_majprix
end type
type rr_1 from roundrectangle within w_majprix
end type
type rr_2 from roundrectangle within w_majprix
end type
end forward

global type w_majprix from window
integer width = 1861
integer height = 2352
boolean titlebar = true
string title = "w_majprix"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 15780518
string icon = "AppIcon!"
boolean center = true
cb_14 cb_14
cb_1 cb_1
cb_13 cb_13
cb_12 cb_12
cb_11 cb_11
cb_10 cb_10
cb_9 cb_9
cb_8 cb_8
cb_7 cb_7
cb_6 cb_6
cb_5 cb_5
cb_4 cb_4
cb_3 cb_3
cb_2 cb_2
cbx_indien cbx_indien
cbx_ramq cbx_ramq
cbx_ass cbx_ass
cbx_pat cbx_pat
cbx_prixlab cbx_prixlab
st_2 st_2
cbx_duree cbx_duree
cbx_prod cbx_prod
cbx_nbcanaux cbx_nbcanaux
cbx_materiel cbx_materiel
cbx_3 cbx_3
cbx_descfr cbx_descfr
cbx_descan cbx_descan
ddplb_guide ddplb_guide
st_guide st_guide
cb_quitter cb_quitter
cb_maj cb_maj
st_soustitre st_soustitre
dw_listeguide dw_listeguide
st_titre st_titre
rr_1 rr_1
rr_2 rr_2
end type
global w_majprix w_majprix

type variables
long il_idguide[]
long il_indice 
transaction lnv_transweb

end variables

forward prototypes
public function any split (string string1, string string2)
public subroutine test ()
public function string remplir (string as_chaine, readonly integer ai_longueur, readonly character ac_car, readonly boolean ab_gauche)
public subroutine uf_importguide ()
end prototypes

public function any split (string string1, string string2);string ls_str[]
long p, i = 1

p = Pos(string1, string2)

Do While p > 0
	ls_str[i] = Left(string1, p - 1)
	i++
	string1 = Mid(string1, p + Len(string2))
	p = Pos(string1, string2)
Loop

ls_str[i] = string1

return ls_str
end function

public subroutine test ();messageBox('Avertissement', "TEST OK", Question!, OK!)
end subroutine

public function string remplir (string as_chaine, readonly integer ai_longueur, readonly character ac_car, readonly boolean ab_gauche);if isNull(as_chaine) or isNull(ai_longueur) or isNull(ac_car) or isNull(ab_gauche) then
	setNull(as_chaine)
	return as_chaine
end if

if len(ac_car) < 1 then return as_chaine

do while len(as_chaine) < ai_longueur
	if ab_gauche then as_chaine = string(ac_car) + as_chaine else as_chaine += string(ac_car)
loop

return as_chaine

end function

public subroutine uf_importguide ();string ls_code, ls_descfr, ls_descan, ls_prod = "", ls_materiel
string ls_fullpath, ls_linefich, ls_titre, ls_type, ls_abs
integer li_FileNum, li_erreur, li_nbcanaux
char lc_prod
long ll_pos, ll_count, ll_duree, ll_idguide, ll_idodontotypemat
dec ld_prix, ld_prixlab // prix prestation
string ls_docpath, ls_docname, ls_tab[]

setPointer(hourglass!)

//if GetFileOpenName("Choisir un fichier",ls_docpath, ls_docname, "CSV", "Text Files (*.CSV),*.CSV,", "C:\ii4net\dentitek", 18) < 1 then return	
li_FileNum = FileOpen("C:\ii4net\dentitek\impguide.csv", LineMode!, Read!, LockReadWrite!)	
li_erreur = FileRead(li_FileNum,ls_linefich)
li_erreur = FileRead(li_FileNum,ls_linefich)
ll_idguide = il_idguide[il_indice]

DO WHILE li_erreur > 0
	
//	w_slapshot.ii_i = w_slapshot.ii_i + 1
	ls_tab = split(ls_linefich,';')
	
	if upperbound(ls_tab) >= 4 then
	
		ls_code = trim(ls_tab[1])
		ls_code = remplir(ls_code,5,"0",true)
		
		ls_descfr = left(ls_tab[2],200)
		ls_titre = left(ls_descfr,100)
		ls_descan = left(ls_tab[3],200)
		ld_prix = dec(ls_tab[4])
		if upperbound(ls_tab) >= 5 then
			ls_materiel = ls_tab[5]
			ll_idodontotypemat = long(ls_materiel)
			if ll_idodontotypemat = 0 then setnull(ll_idodontotypemat)
		else
			setnull(ls_materiel)
			setnull(ll_idodontotypemat)
		end if
		if upperbound(ls_tab) >= 6 then
			li_nbcanaux = integer(ls_tab[6])
		else
			setnull(li_nbcanaux)
		end if
		if upperbound(ls_tab) >= 7 then
			ld_prixlab = dec(ls_tab[7])
		else
			ld_prixlab = 0
		end if
		if upperbound(ls_tab) >= 8 then
			lc_prod = ls_tab[8]
		else
			setnull(lc_prod)
		end if
		if upperbound(ls_tab) >= 9 then
			ll_duree = long(ls_tab[9])
		else
			setnull(ll_duree)
		end if
		
		// Gestion des critères de base
		
		select count(*) into :ll_count from t_codes where id_code = :ls_code and id_guide = :ll_idguide using sqlca;
		if ll_count = 1 then
			
			if cbx_descfr.checked then
				update t_codes set description = :ls_descfr
				where id_code = :ls_code and id_guide = :il_indice using sqlca;
				if SQLCA.SQLCOde <> 0 then
					messagebox(string(SQLCA.SQLCOde),SQLCA.SQLerrText)
				end if
			end if
			
			if cbx_descan.checked then
				update t_codes set descran = :ls_descan
				where id_code = :ls_code and id_guide = :il_indice using sqlca;
				if SQLCA.SQLCOde <> 0 then
					messagebox(string(SQLCA.SQLCOde),SQLCA.SQLerrText)
				end if
			end if
			
		else
			if ls_code <> '00000' then
				ls_prod += ls_code + ' '
				insert into t_codes(id_code,codeuser,description,descran,prixcas,prixramq,titre,prixlab,id_guide,prixass) 
				values(:ls_code,:ls_code,:ls_descfr,:ls_descan,0.00,0.00,:ls_titre,0.00,:ll_idguide,0.00) using sqlca;
				if SQLCA.SQLCOde <> 0 then
					messagebox(string(SQLCA.SQLCOde),SQLCA.SQLerrText)
				end if
			end if
		end if
		
		// Affectation des prix CAS
		if cbx_pat.checked then 
			update t_codes set prixcas = :ld_prix, codepat = 1 where id_code = :ls_code and id_guide = :ll_idguide using sqlca;
			if SQLCA.SQLCOde <> 0 then
				messagebox(string(SQLCA.SQLCOde),SQLCA.SQLerrText)
			end if
		end if
		
		// Affectation des prix ASS
		if cbx_ass.checked then 
			update t_codes set prixass = :ld_prix, codeass = 1 where id_code = :ls_code and id_guide = :ll_idguide using sqlca;
			if SQLCA.SQLCOde <> 0 then
				messagebox(string(SQLCA.SQLCOde),SQLCA.SQLerrText)
			end if
		end if
		
		// Affectation des prix AMQ
		if cbx_ramq.checked then 
			update t_codes set prixramq = :ld_prix, coderamq = 1 where id_code = :ls_code and id_guide = :ll_idguide using sqlca;
			if SQLCA.SQLCOde <> 0 then
				messagebox(string(SQLCA.SQLCOde),SQLCA.SQLerrText)
			end if
		end if
		
		// Affectation des prix indiens
		if cbx_indien.checked then 
			update t_codes set prixindien = :ld_prix, codepat = 1 where id_code = :ls_code and id_guide = :ll_idguide using sqlca; 
			if SQLCA.SQLCOde <> 0 then
				messagebox(string(SQLCA.SQLCOde),SQLCA.SQLerrText)
			end if
		end if
		
		// Affectation des materiaux
		if cbx_materiel.checked then 
			update t_codes set id_odontotypemat = :ll_idodontotypemat where id_code = :ls_code and id_guide = :ll_idguide using sqlca; 
			if SQLCA.SQLCOde <> 0 then
				messagebox(string(SQLCA.SQLCOde) + " - " + string(ll_idodontotypemat),SQLCA.SQLerrText)
			end if
		end if
		
		// Affectation des canaux
		if cbx_nbcanaux.checked then 
			update t_codes set nbcanaux = :li_nbcanaux where id_code = :ls_code and id_guide = :ll_idguide using sqlca; 
			if SQLCA.SQLCOde <> 0 then
				messagebox(string(SQLCA.SQLCOde),SQLCA.SQLerrText)
			end if
		end if
		
		// Affectation du prix de laboratoire
		if cbx_prixlab.checked then
			update t_codes set prixlab = :ld_prixlab where id_code = :ls_code and id_guide = :ll_idguide using sqlca; 
			if SQLCA.SQLCOde <> 0 then
				messagebox(string(SQLCA.SQLCOde),SQLCA.SQLerrText)
			end if
		end if
		
		// Affectation des producteurs
		if cbx_prod.checked then
			update t_codes set prod = :lc_prod where id_code = :ls_code and id_guide = :ll_idguide using sqlca; 
			if SQLCA.SQLCOde <> 0 then
				messagebox(string(SQLCA.SQLCOde),SQLCA.SQLerrText)
			end if
		end if
		
		// Affectation des producteurs
		if cbx_duree.checked then
			update t_codes set dureeregie = :ll_duree where id_code = :ls_code and id_guide = :ll_idguide using sqlca; 
			if SQLCA.SQLCOde <> 0 then
				messagebox(string(SQLCA.SQLCOde),SQLCA.SQLerrText)
			end if
		end if
	end if
	li_erreur = FileRead(li_FileNum,ls_linefich)
	
LOOP

//DO WHILE  li_erreur > 0
//	// On va chercher les variable de la ligne courante dans le fichier
//
//	ls_tab = split(ls_linefich,';')
//	ls_code = trim(ls_tab[1])
//	ls_code = remplir(ls_code,5,"0",true)
//	ls_descfr = ls_tab[2]
//	ls_descan = ls_tab[3]
//	ld_prix = dec(ls_tab[5])
//	ls_titre = ls_tab[2]
//	if (upperbound(ls_tab) > 5) then
//		ls_materiel = ls_tab[6]
//		if (upperbound(ls_tab) > 6) then
//			li_nbcanaux = integer(ls_tab[7])
//		end if
//	end if
//	
//	// On vérifie si le code est déjà inscrit dans la base si oui
//	// on modifie le prix sinon on ajoute le nouveau code avec les 
//	// information
//	
//	select count(*) into :ll_count from t_codes where id_code = :ls_code and id_guide = :il_indice using sqlca;
//	if ll_count = 1 then
//		update t_codes set description = :ls_descfr, descran = :ls_descan, prixcas = :ld_prix, prixass = :ld_prix, typemateriel = :ls_materiel, nbcanaux = :li_nbcanaux, codepat=1, codeass=1 where id_code = :ls_code and id_guide = :il_indice using sqlca;
//	else
//		if ls_code <> '00000' then
//			ls_prod += ls_code + ' '
//			insert into t_codes(id_code,codeuser,description,descran,prixcas,prixramq,titre,prixlab,id_guide,prixass,codeass,codepat,typemateriel,nbcanaux) 
//			values(:ls_code,:ls_code,:ls_descfr,:ls_descan,:ld_prix,0.00,:ls_titre,0.00,:il_indice,:ld_prix,1,1,:ls_materiel,:li_nbcanaux) using sqlca;
//		end if
//	end if
	
//	commit using SQLCA;
//	li_erreur = FileRead(li_FileNum,ls_linefich)
//LOOP
FileClose(li_FileNum)
messagebox('Information', + "La base a été mise à jour / Database updated ",Information!,Ok!)
if ls_prod <> '' then
		messagebox('Avertissement', + "Les codes suivants n'ont pas de producteur: / The following codes do not have any productor : " + ls_prod,Information!,Ok!)
end if
end subroutine

on w_majprix.create
this.cb_14=create cb_14
this.cb_1=create cb_1
this.cb_13=create cb_13
this.cb_12=create cb_12
this.cb_11=create cb_11
this.cb_10=create cb_10
this.cb_9=create cb_9
this.cb_8=create cb_8
this.cb_7=create cb_7
this.cb_6=create cb_6
this.cb_5=create cb_5
this.cb_4=create cb_4
this.cb_3=create cb_3
this.cb_2=create cb_2
this.cbx_indien=create cbx_indien
this.cbx_ramq=create cbx_ramq
this.cbx_ass=create cbx_ass
this.cbx_pat=create cbx_pat
this.cbx_prixlab=create cbx_prixlab
this.st_2=create st_2
this.cbx_duree=create cbx_duree
this.cbx_prod=create cbx_prod
this.cbx_nbcanaux=create cbx_nbcanaux
this.cbx_materiel=create cbx_materiel
this.cbx_3=create cbx_3
this.cbx_descfr=create cbx_descfr
this.cbx_descan=create cbx_descan
this.ddplb_guide=create ddplb_guide
this.st_guide=create st_guide
this.cb_quitter=create cb_quitter
this.cb_maj=create cb_maj
this.st_soustitre=create st_soustitre
this.dw_listeguide=create dw_listeguide
this.st_titre=create st_titre
this.rr_1=create rr_1
this.rr_2=create rr_2
this.Control[]={this.cb_14,&
this.cb_1,&
this.cb_13,&
this.cb_12,&
this.cb_11,&
this.cb_10,&
this.cb_9,&
this.cb_8,&
this.cb_7,&
this.cb_6,&
this.cb_5,&
this.cb_4,&
this.cb_3,&
this.cb_2,&
this.cbx_indien,&
this.cbx_ramq,&
this.cbx_ass,&
this.cbx_pat,&
this.cbx_prixlab,&
this.st_2,&
this.cbx_duree,&
this.cbx_prod,&
this.cbx_nbcanaux,&
this.cbx_materiel,&
this.cbx_3,&
this.cbx_descfr,&
this.cbx_descan,&
this.ddplb_guide,&
this.st_guide,&
this.cb_quitter,&
this.cb_maj,&
this.st_soustitre,&
this.dw_listeguide,&
this.st_titre,&
this.rr_1,&
this.rr_2}
end on

on w_majprix.destroy
destroy(this.cb_14)
destroy(this.cb_1)
destroy(this.cb_13)
destroy(this.cb_12)
destroy(this.cb_11)
destroy(this.cb_10)
destroy(this.cb_9)
destroy(this.cb_8)
destroy(this.cb_7)
destroy(this.cb_6)
destroy(this.cb_5)
destroy(this.cb_4)
destroy(this.cb_3)
destroy(this.cb_2)
destroy(this.cbx_indien)
destroy(this.cbx_ramq)
destroy(this.cbx_ass)
destroy(this.cbx_pat)
destroy(this.cbx_prixlab)
destroy(this.st_2)
destroy(this.cbx_duree)
destroy(this.cbx_prod)
destroy(this.cbx_nbcanaux)
destroy(this.cbx_materiel)
destroy(this.cbx_3)
destroy(this.cbx_descfr)
destroy(this.cbx_descan)
destroy(this.ddplb_guide)
destroy(this.st_guide)
destroy(this.cb_quitter)
destroy(this.cb_maj)
destroy(this.st_soustitre)
destroy(this.dw_listeguide)
destroy(this.st_titre)
destroy(this.rr_1)
destroy(this.rr_2)
end on

event open;lnv_transweb = create transaction
// Profile progitek_web
lnv_transweb.DBMS = "OLE DB"
lnv_transweb.LogPass = "10progitek01"
lnv_transweb.LogId = "progitek"
lnv_transweb.AutoCommit = False
lnv_transweb.DBParm = "PROVIDER='SQLOLEDB',DATASOURCE='www.atmtech.biz',PROVIDERSTRING='database=progitek;'"

connect using lnv_transweb;
if lnv_transweb.sqlcode <> 0 then
	MessageBox("Information", "Connexion à la base impossible. Veuillez contacter Progitek / Cannot connect to database. Please contact Progitek " +lnv_transweb.sqlerrtext)
	//return
end if

//dw_listeguide.setTransObject(lnv_transweb)
//dw_listeguide.retrieve()
//dw_listeguide.SelectRow(5,true)
//dw_listeguide.scrollTorow(5)
//
//cb_maj.event clicked()

end event

event close;close(w_slapshot)
end event

type cb_14 from commandbutton within w_majprix
integer x = 64
integer y = 1948
integer width = 1701
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Bien-Être Nouveau Brunswick"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = true
cbx_ass.checked = false
cbx_pat.checked = false

ll_idtemp =  14
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_1 from commandbutton within w_majprix
integer x = 64
integer y = 1832
integer width = 1701
integer height = 112
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "PRIX SSNA Nouveau Brunswick"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = true
cbx_ass.checked = false
cbx_pat.checked = false

ll_idtemp =  13
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_13 from commandbutton within w_majprix
integer x = 64
integer y = 1716
integer width = 1701
integer height = 112
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Chirurgien maxillo-faciale RAMQ"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = true
cbx_ass.checked = false
cbx_pat.checked = false

ll_idtemp =  12
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_12 from commandbutton within w_majprix
integer x = 64
integer y = 1596
integer width = 1701
integer height = 112
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Chirurgien maxillo-faciale"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = false
cbx_ass.checked = true
cbx_pat.checked = true

ll_idtemp =  11
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_11 from commandbutton within w_majprix
integer x = 64
integer y = 1476
integer width = 1701
integer height = 112
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Prosthodontie"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = false
cbx_ass.checked = true
cbx_pat.checked = true

ll_idtemp =  10
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_10 from commandbutton within w_majprix
integer x = 64
integer y = 1356
integer width = 1701
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Pédodontie"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = false
cbx_ass.checked = true
cbx_pat.checked = true

ll_idtemp =  9
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_9 from commandbutton within w_majprix
integer x = 64
integer y = 1236
integer width = 1701
integer height = 112
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Parodontie"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = false
cbx_ass.checked = true
cbx_pat.checked = true

ll_idtemp =  8
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_8 from commandbutton within w_majprix
integer x = 64
integer y = 1116
integer width = 1701
integer height = 112
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Endodontie"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = false
cbx_ass.checked = true
cbx_pat.checked = true

ll_idtemp =  7
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_7 from commandbutton within w_majprix
integer x = 64
integer y = 996
integer width = 1701
integer height = 112
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Prix Denturo Nouveau-Brunswick"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = false
cbx_ass.checked = true
cbx_pat.checked = true

ll_idtemp =  6
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_6 from commandbutton within w_majprix
integer x = 64
integer y = 876
integer width = 1701
integer height = 112
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Prix Denturo"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = false
cbx_ass.checked = true
cbx_pat.checked = true

ll_idtemp =  5
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_5 from commandbutton within w_majprix
integer x = 64
integer y = 756
integer width = 1701
integer height = 112
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Prix Nouveau-Brunswick"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = false
cbx_ass.checked = true
cbx_pat.checked = true

ll_idtemp =  4
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_4 from commandbutton within w_majprix
integer x = 64
integer y = 636
integer width = 1701
integer height = 112
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Prix SSNA"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = true
cbx_ramq.checked = false
cbx_ass.checked = false
cbx_pat.checked = false

ll_idtemp =  3
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_3 from commandbutton within w_majprix
integer x = 59
integer y = 516
integer width = 1701
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Prix RAMQ"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = true
cbx_ass.checked = false
cbx_pat.checked = false

ll_idtemp =  2
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cb_2 from commandbutton within w_majprix
integer x = 59
integer y = 396
integer width = 1701
integer height = 112
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Prix ACDQ"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

// Cocher les affaires nécessaires

cbx_indien.checked = false
cbx_ramq.checked = false
cbx_ass.checked = true
cbx_pat.checked = true

ll_idtemp =  1
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_guide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type cbx_indien from checkbox within w_majprix
integer x = 1829
integer y = 876
integer width = 398
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "Indien"
end type

type cbx_ramq from checkbox within w_majprix
integer x = 2245
integer y = 872
integer width = 398
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "RAMQ"
end type

type cbx_ass from checkbox within w_majprix
integer x = 2638
integer y = 872
integer width = 398
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "Assurance"
end type

type cbx_pat from checkbox within w_majprix
integer x = 3045
integer y = 876
integer width = 398
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "Patient"
end type

type cbx_prixlab from checkbox within w_majprix
integer x = 1851
integer y = 996
integer width = 1413
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "Frais de laboratoire  / Laboratory fees"
end type

type st_2 from statictext within w_majprix
integer x = 1906
integer y = 192
integer width = 1001
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "Champs à importer / Fields to import "
boolean focusrectangle = false
end type

type cbx_duree from checkbox within w_majprix
integer x = 1856
integer y = 1536
integer width = 1413
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "Durée pour régie / Lengh for RAMQ"
end type

type cbx_prod from checkbox within w_majprix
integer x = 1842
integer y = 1432
integer width = 1413
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "Producteur / Producer "
end type

type cbx_nbcanaux from checkbox within w_majprix
integer x = 1838
integer y = 1216
integer width = 1413
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "Nbr. canaux / Number of roots"
end type

type cbx_materiel from checkbox within w_majprix
integer x = 1879
integer y = 1732
integer width = 1413
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "Matériaux / Materials"
end type

type cbx_3 from checkbox within w_majprix
integer x = 1883
integer y = 1632
integer width = 1413
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "Tarifs /  Fees"
boolean checked = true
end type

type cbx_descfr from checkbox within w_majprix
integer x = 1847
integer y = 1112
integer width = 1413
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "Description française  / French description "
end type

type cbx_descan from checkbox within w_majprix
integer x = 1819
integer y = 1324
integer width = 1413
integer height = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 12639424
string text = "Description anglaise / English description"
end type

type ddplb_guide from dropdownpicturelistbox within w_majprix
integer x = 398
integer y = 136
integer width = 1417
integer height = 500
integer taborder = 10
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean autohscroll = true
boolean sorted = false
borderstyle borderstyle = stylelowered!
long picturemaskcolor = 536870912
end type

event constructor;long ll_idguide, i=1
string ls_guide

DECLARE listguide CURSOR FOR
	select id_guide, nomguide FROM t_guide;

OPEN listguide;

FETCH listguide INTO :ll_idguide, :ls_guide;

DO WHILE SQLCA.SQLCode = 0
	
	addItem(ls_guide)
	il_idguide[i] = ll_idguide
	i++
	
	FETCH listguide INTO :ll_idguide, :ls_guide;
	
LOOP

CLOSE listguide;

selectItem(1)
il_indice=1


end event

event selectionchanged;il_indice = index
end event

type st_guide from statictext within w_majprix
integer x = 50
integer y = 144
integer width = 315
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 15780518
string text = "Guide :"
boolean focusrectangle = false
end type

type cb_quitter from commandbutton within w_majprix
integer x = 37
integer y = 2100
integer width = 1755
integer height = 108
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Quitter / Quit"
end type

event clicked;halt close
end event

type cb_maj from commandbutton within w_majprix
boolean visible = false
integer x = 1851
integer y = 744
integer width = 1774
integer height = 108
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Mettre à jour / Update"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum
long ll_idtemp

ll_idtemp =  dw_listeguide.getItemNumber(dw_listeguide.getRow(),'id_majguide')
ls_typeguide = dw_listeguide.getItemString(dw_listeguide.getRow(),'typeimportation')
// res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)
res = 1

IF res = 1 THEN
	//Récupérer le fichier de la base de données web
	SELECTBLOB fichier
	INTO :lblob_contenu
	FROM t_majguide where id_majguide = :ll_idtemp using lnv_transweb;
	IF lnv_transweb.SQLCode < 0 THEN
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		RETURN 0
	END IF	
	if Len(lblob_contenu) <= 0 then
		messagebox("Attention", "Erreur, veuillez contactez Progitek / Error, please contact Progitek " +  lnv_transweb.sqlerrtext)
		return 0
	end if
	
	SetPointer(HourGlass!)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF

close(parent)
end event

type st_soustitre from statictext within w_majprix
integer x = 41
integer y = 256
integer width = 1691
integer height = 104
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 15780518
string text = "Cliquer sur le bouton du guide que vous voulez mettre à jour"
boolean focusrectangle = false
end type

type dw_listeguide from datawindow within w_majprix
integer x = 1970
integer y = 1832
integer width = 187
integer height = 140
integer taborder = 10
string title = "none"
string dataobject = "d_listeguide"
boolean minbox = true
boolean vscrollbar = true
boolean border = false
string icon = "DataWindow5!"
boolean livescroll = true
end type

event clicked;long i = row
this.selectRow(i,false)
this.selectRow(row,true)

// évite la sélection de plusieurs lignes
if row = 0 then
	this.selectRow(row,false)
	this.selectRow(1, true)
end if
end event

type st_titre from statictext within w_majprix
integer x = 41
integer y = 16
integer width = 1765
integer height = 80
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 15793151
string text = "Mise à jour du guide des prix // Guide prices update"
alignment alignment = center!
boolean focusrectangle = false
end type

type rr_1 from roundrectangle within w_majprix
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 1073741824
integer x = 18
integer y = 12
integer width = 1801
integer height = 92
integer cornerheight = 40
integer cornerwidth = 46
end type

type rr_2 from roundrectangle within w_majprix
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 1073741824
integer x = 18
integer y = 372
integer width = 1797
integer height = 1720
integer cornerheight = 40
integer cornerwidth = 46
end type

