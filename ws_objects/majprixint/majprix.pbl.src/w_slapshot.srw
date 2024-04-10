$PBExportHeader$w_slapshot.srw
forward
global type w_slapshot from window
end type
type p_1 from picture within w_slapshot
end type
type hpb_prog from hprogressbar within w_slapshot
end type
type st_1 from statictext within w_slapshot
end type
end forward

global type w_slapshot from window
integer width = 2295
integer height = 1096
boolean titlebar = true
boolean controlmenu = true
boolean minbox = true
windowtype windowtype = popup!
long backcolor = 12639424
string icon = "AppIcon!"
boolean center = true
p_1 p_1
hpb_prog hpb_prog
st_1 st_1
end type
global w_slapshot w_slapshot

type variables
long ii_i = 1
end variables

on w_slapshot.create
this.p_1=create p_1
this.hpb_prog=create hpb_prog
this.st_1=create st_1
this.Control[]={this.p_1,&
this.hpb_prog,&
this.st_1}
end on

on w_slapshot.destroy
destroy(this.p_1)
destroy(this.hpb_prog)
destroy(this.st_1)
end on

event open;timer(1)
end event

type p_1 from picture within w_slapshot
integer x = 27
integer y = 32
integer width = 2199
integer height = 276
boolean originalsize = true
string picturename = "C:\ii4net\dentitek\images\progitek.PNG"
boolean border = true
boolean focusrectangle = false
end type

type hpb_prog from hprogressbar within w_slapshot
integer x = 9
integer y = 936
integer width = 2254
integer height = 68
unsignedinteger maxposition = 60
unsignedinteger position = 1
integer setstep = 1
end type

type st_1 from statictext within w_slapshot
integer x = 27
integer y = 372
integer width = 2199
integer height = 444
integer textsize = -24
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 12639424
string text = "Mise-à-jour des tarifs"
alignment alignment = center!
boolean border = true
boolean focusrectangle = false
end type

