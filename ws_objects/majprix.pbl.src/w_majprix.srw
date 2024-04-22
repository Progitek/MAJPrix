$PBExportHeader$w_majprix.srw
forward
global type w_majprix from window
end type
type sle_odbc from singlelineedit within w_majprix
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
type cb_1 from commandbutton within w_majprix
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
type st_1 from statictext within w_majprix
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
type rr_3 from roundrectangle within w_majprix
end type
end forward

global type w_majprix from window
integer width = 3538
integer height = 2116
boolean titlebar = true
string title = "w_majprix"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 15780518
string icon = "AppIcon!"
boolean center = true
sle_odbc sle_odbc
cb_2 cb_2
cbx_indien cbx_indien
cbx_ramq cbx_ramq
cbx_ass cbx_ass
cbx_pat cbx_pat
cbx_prixlab cbx_prixlab
st_2 st_2
cb_1 cb_1
cbx_duree cbx_duree
cbx_prod cbx_prod
cbx_nbcanaux cbx_nbcanaux
cbx_materiel cbx_materiel
cbx_3 cbx_3
cbx_descfr cbx_descfr
cbx_descan cbx_descan
st_1 st_1
ddplb_guide ddplb_guide
st_guide st_guide
cb_quitter cb_quitter
cb_maj cb_maj
st_soustitre st_soustitre
dw_listeguide dw_listeguide
st_titre st_titre
rr_1 rr_1
rr_2 rr_2
rr_3 rr_3
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
string ls_fullpath, ls_linefich, ls_titre, ls_type, ls_abs, ls_codeuser
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
		ls_codeuser = left(ls_code,5)
		
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
				values(:ls_code,:ls_codeuser,:ls_descfr,:ls_descan,0.00,0.00,:ls_titre,0.00,:ll_idguide,0.00) using sqlca;
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
this.sle_odbc=create sle_odbc
this.cb_2=create cb_2
this.cbx_indien=create cbx_indien
this.cbx_ramq=create cbx_ramq
this.cbx_ass=create cbx_ass
this.cbx_pat=create cbx_pat
this.cbx_prixlab=create cbx_prixlab
this.st_2=create st_2
this.cb_1=create cb_1
this.cbx_duree=create cbx_duree
this.cbx_prod=create cbx_prod
this.cbx_nbcanaux=create cbx_nbcanaux
this.cbx_materiel=create cbx_materiel
this.cbx_3=create cbx_3
this.cbx_descfr=create cbx_descfr
this.cbx_descan=create cbx_descan
this.st_1=create st_1
this.ddplb_guide=create ddplb_guide
this.st_guide=create st_guide
this.cb_quitter=create cb_quitter
this.cb_maj=create cb_maj
this.st_soustitre=create st_soustitre
this.dw_listeguide=create dw_listeguide
this.st_titre=create st_titre
this.rr_1=create rr_1
this.rr_2=create rr_2
this.rr_3=create rr_3
this.Control[]={this.sle_odbc,&
this.cb_2,&
this.cbx_indien,&
this.cbx_ramq,&
this.cbx_ass,&
this.cbx_pat,&
this.cbx_prixlab,&
this.st_2,&
this.cb_1,&
this.cbx_duree,&
this.cbx_prod,&
this.cbx_nbcanaux,&
this.cbx_materiel,&
this.cbx_3,&
this.cbx_descfr,&
this.cbx_descan,&
this.st_1,&
this.ddplb_guide,&
this.st_guide,&
this.cb_quitter,&
this.cb_maj,&
this.st_soustitre,&
this.dw_listeguide,&
this.st_titre,&
this.rr_1,&
this.rr_2,&
this.rr_3}
end on

on w_majprix.destroy
destroy(this.sle_odbc)
destroy(this.cb_2)
destroy(this.cbx_indien)
destroy(this.cbx_ramq)
destroy(this.cbx_ass)
destroy(this.cbx_pat)
destroy(this.cbx_prixlab)
destroy(this.st_2)
destroy(this.cb_1)
destroy(this.cbx_duree)
destroy(this.cbx_prod)
destroy(this.cbx_nbcanaux)
destroy(this.cbx_materiel)
destroy(this.cbx_3)
destroy(this.cbx_descfr)
destroy(this.cbx_descan)
destroy(this.st_1)
destroy(this.ddplb_guide)
destroy(this.st_guide)
destroy(this.cb_quitter)
destroy(this.cb_maj)
destroy(this.st_soustitre)
destroy(this.dw_listeguide)
destroy(this.st_titre)
destroy(this.rr_1)
destroy(this.rr_2)
destroy(this.rr_3)
end on

event open;lnv_transweb = create transaction
// Profile progitek_web
lnv_transweb.DBMS = "OLE DB"
lnv_transweb.AutoCommit = False

// MAP 2024-04-22 DENT-2995 Modifier l'exécutable PB Mise_A_Jour_Prix via ProgitekUtility
//lnv_transweb.LogPass = "10progitek01"
//lnv_transweb.LogId = "progitek"
// lnv_transweb.DBParm = "PROVIDER='SQLOLEDB',DATASOURCE='www.atmtech.biz',PROVIDERSTRING='database=progitek;'"
lnv_transweb.LogPass = "VK59t6[(05QUbmcog1FrbNC]S"
lnv_transweb.LogId = "sQdZJAkySsVu8u3h"
lnv_transweb.DBParm = "PROVIDER='SQLOLEDB',DATASOURCE='legacy-guide.dentitek.ca',PROVIDERSTRING='database=progitek;'"

connect using lnv_transweb;
if lnv_transweb.sqlcode <> 0 then
	MessageBox("Information", "Connexion à la base impossible. Veuillez contacter Progitek / Cannot connect to database. Please contact Progitek " +lnv_transweb.sqlerrtext)
	return
end if

dw_listeguide.setTransObject(lnv_transweb)
dw_listeguide.retrieve()
dw_listeguide.SelectRow(4,true)
dw_listeguide.scrollTorow(4)
end event

type sle_odbc from singlelineedit within w_majprix
integer x = 2597
integer y = 1900
integer width = 352
integer height = 92
integer taborder = 50
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type cb_2 from commandbutton within w_majprix
integer x = 2181
integer y = 1896
integer width = 402
integer height = 112
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Connecter"
end type

event clicked;string ls_odbc, ls_sql

ls_odbc = sle_odbc.text
SQLCA.DbParm  = "ConnectString='DSN="+ ls_odbc +";UID=dba;PWD=ii4pr0g1+3k01',ConnectOption='SQL_DRIVER_CONNECT,SQL_DRIVER_NOPROMPT'"

connect using SQLCA;
if sqlca.sqlcode <> 0 then
	MessageBox ("Impossible de se connecter à la base de données / Cannot Connect to Database", sqlca.sqlerrtext)
	return
else
	MessageBox ("Connection réussi avec succès", sqlca.sqlerrtext)
end if

// Authentifier la connection pour la version 11

ls_sql = "SET TEMPORARY OPTION CONNECTION_AUTHENTICATION='Company=Progitek;Application=Progitek;Signature=000fa55157edb8e14d818eb4fe3db41447146f1571g7cf6b1c162e7a4e4925570fc8104c82ac6d466c6'"
execute immediate :ls_sql using SQLCA;
if sqlca.sqlcode <> 0 then
	MessageBox ("Validation d'authentification", sqlca.sqlerrtext)
	return
end if
end event

type cbx_indien from checkbox within w_majprix
integer x = 3072
integer y = 1248
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
integer x = 2702
integer y = 1248
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
integer x = 2272
integer y = 1248
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
boolean checked = true
end type

type cbx_pat from checkbox within w_majprix
integer x = 1847
integer y = 1248
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
boolean checked = true
end type

type cbx_prixlab from checkbox within w_majprix
integer x = 1847
integer y = 1376
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
integer x = 50
integer y = 1248
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

type cb_1 from commandbutton within w_majprix
integer x = 1102
integer y = 1896
integer width = 1083
integer height = 108
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Fichier - Mettre à jour / Update"
end type

event clicked;integer res // résultat du messagebox
blob lblob_contenu
string ls_typeguide // type du guide sélectionné dans la datawindow
integer li_FileNum, li_erreur
long ll_idtemp
string ls_docpath, ls_docname

res = messagebox('Avertissement', "Voulez-vous réellement faire la mise à jour ? / Do you really want to update ?", Question!, YesNo!)

IF res = 1 THEN
	
	GetFileOpenName("Choisir un fichier",ls_docpath, ls_docname, "CSV", "Text Files (*.CSV),*.CSV,", "C:\ii4net\dentitek", 18)
	li_FileNum = FileOpen(ls_docpath, StreamMode!, Read!, LockRead!)
	li_erreur = FileReadEx(li_FileNum,lblob_contenu)
	
	//Écrire le fichier
	li_FileNum = FileOpen( "C:\ii4net\dentitek\impguide.csv", StreamMode!, Write!,LockWrite!, Replace!)
	FileWriteEx(li_FileNum, lblob_contenu)
	FileClose(li_FileNum)
	uf_importguide()
	
	// Suppression du fichier sur le disque
	FileDelete("C:\ii4net\dentitek\impguide.csv")
END IF


end event

type cbx_duree from checkbox within w_majprix
integer x = 1847
integer y = 1760
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
integer x = 1847
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
string text = "Producteur / Producer "
end type

type cbx_nbcanaux from checkbox within w_majprix
integer x = 1847
integer y = 1504
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
integer x = 133
integer y = 1760
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
integer x = 133
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
integer x = 133
integer y = 1376
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
integer x = 133
integer y = 1504
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

type st_1 from statictext within w_majprix
integer x = 41
integer y = 220
integer width = 974
integer height = 64
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 15793151
string text = "Select the guide you want to update."
boolean focusrectangle = false
end type

type ddplb_guide from dropdownpicturelistbox within w_majprix
integer x = 2459
integer y = 140
integer width = 1015
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
integer x = 2098
integer y = 152
integer width = 517
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 15793151
string text = "Guide :"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_quitter from commandbutton within w_majprix
integer x = 2967
integer y = 1896
integer width = 526
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
integer x = 18
integer y = 1896
integer width = 1083
integer height = 108
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Internet - Mettre à jour / Update"
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
integer y = 152
integer width = 1879
integer height = 104
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean italic = true
long textcolor = 33554432
long backcolor = 15793151
string text = "Sélectionner le guide pour effecuter la mise à jour."
boolean focusrectangle = false
end type

type dw_listeguide from datawindow within w_majprix
integer x = 37
integer y = 352
integer width = 3429
integer height = 800
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
integer width = 3401
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
integer width = 3465
integer height = 92
integer cornerheight = 40
integer cornerwidth = 46
end type

type rr_2 from roundrectangle within w_majprix
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 1073741824
integer x = 18
integer y = 116
integer width = 3465
integer height = 1100
integer cornerheight = 40
integer cornerwidth = 46
end type

type rr_3 from roundrectangle within w_majprix
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 12639424
integer x = 18
integer y = 1228
integer width = 3465
integer height = 660
integer cornerheight = 40
integer cornerwidth = 46
end type

