{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "studio.h"
module Studio where
import Foreign.Ptr
#strict_import

{- typedef struct {
            float x, y, z;
        } vec3_t; -}
#starttype vec3_t
#field x , CFloat
#field y , CFloat
#field z , CFloat
#stoptype
{- typedef struct {
            int id;
            int version;
            char name[64];
            int length;
            vec3_t eyeposition;
            vec3_t min;
            vec3_t max;
            vec3_t bbmin;
            vec3_t bbmax;
            int flags;
            int numbones;
            int boneindex;
            int numbonecontrollers;
            int bonecontrollerindex;
            int numhitboxes;
            int hitboxindex;
            int numseq;
            int seqindex;
            int numseqgroups;
            int seqgroupindex;
            int numtextures;
            int textureindex;
            int texturedataindex;
            int numskinref;
            int numskinfamilies;
            int skinindex;
            int numbodyparts;
            int bodypartindex;
            int numattachments;
            int attachmentindex;
            int soundtable;
            int soundindex;
            int soundgroups;
            int soundgroupindex;
            int numtransitions;
            int transitionindex;
        } studiohdr_t; -}
#starttype studiohdr_t
#field id , CInt
#field version , CInt
#array_field name , CChar
#field length , CInt
#field eyeposition , <vec3_t>
#field min , <vec3_t>
#field max , <vec3_t>
#field bbmin , <vec3_t>
#field bbmax , <vec3_t>
#field flags , CInt
#field numbones , CInt
#field boneindex , CInt
#field numbonecontrollers , CInt
#field bonecontrollerindex , CInt
#field numhitboxes , CInt
#field hitboxindex , CInt
#field numseq , CInt
#field seqindex , CInt
#field numseqgroups , CInt
#field seqgroupindex , CInt
#field numtextures , CInt
#field textureindex , CInt
#field texturedataindex , CInt
#field numskinref , CInt
#field numskinfamilies , CInt
#field skinindex , CInt
#field numbodyparts , CInt
#field bodypartindex , CInt
#field numattachments , CInt
#field attachmentindex , CInt
#field soundtable , CInt
#field soundindex , CInt
#field soundgroups , CInt
#field soundgroupindex , CInt
#field numtransitions , CInt
#field transitionindex , CInt
#stoptype
{- typedef struct {
            int id; int version; char name[64]; int length;
        } studioseqhdr_t; -}
#starttype studioseqhdr_t
#field id , CInt
#field version , CInt
#array_field name , CChar
#field length , CInt
#stoptype
{- typedef struct {
            char name[32];
            int parent;
            int flags;
            int bonecontroller[6];
            float value[6];
            float scale[6];
        } mstudiobone_t; -}
#starttype mstudiobone_t
#array_field name , CChar
#field parent , CInt
#field flags , CInt
#array_field bonecontroller , CInt
#array_field value , CFloat
#array_field scale , CFloat
#stoptype
{- typedef struct {
            int bone; int type; float start; float end; int rest; int index;
        } mstudiobonecontroller_t; -}
#starttype mstudiobonecontroller_t
#field bone , CInt
#field type , CInt
#field start , CFloat
#field end , CFloat
#field rest , CInt
#field index , CInt
#stoptype
{- typedef struct {
            int bone; int group; vec3_t bbmin; vec3_t bbmax;
        } mstudiobbox_t; -}
#starttype mstudiobbox_t
#field bone , CInt
#field group , CInt
#field bbmin , <vec3_t>
#field bbmax , <vec3_t>
#stoptype
{- typedef struct cache_user_s {
            void * data;
        } cache_user_t; -}
#starttype struct cache_user_s
#field data , Ptr ()
#stoptype
#synonym_t cache_user_t , <struct cache_user_s>
{- typedef struct {
            char label[32]; char name[64]; int unused1; int unused2;
        } mstudioseqgroup_t; -}
#starttype mstudioseqgroup_t
#array_field label , CChar
#array_field name , CChar
#field unused1 , CInt
#field unused2 , CInt
#stoptype
{- typedef struct {
            char label[32];
            float fps;
            int flags;
            int activity;
            int actweight;
            int numevents;
            int eventindex;
            int numframes;
            int numpivots;
            int pivotindex;
            int motiontype;
            int motionbone;
            vec3_t linearmovement;
            int automoveposindex;
            int automoveangleindex;
            vec3_t bbmin;
            vec3_t bbmax;
            int numblends;
            int animindex;
            int blendtype[2];
            float blendstart[2];
            float blendend[2];
            int blendparent;
            int seqgroup;
            int entrynode;
            int exitnode;
            int nodeflags;
            int nextseq;
        } mstudioseqdesc_t; -}
#starttype mstudioseqdesc_t
#array_field label , CChar
#field fps , CFloat
#field flags , CInt
#field activity , CInt
#field actweight , CInt
#field numevents , CInt
#field eventindex , CInt
#field numframes , CInt
#field numpivots , CInt
#field pivotindex , CInt
#field motiontype , CInt
#field motionbone , CInt
#field linearmovement , <vec3_t>
#field automoveposindex , CInt
#field automoveangleindex , CInt
#field bbmin , <vec3_t>
#field bbmax , <vec3_t>
#field numblends , CInt
#field animindex , CInt
#array_field blendtype , CInt
#array_field blendstart , CFloat
#array_field blendend , CFloat
#field blendparent , CInt
#field seqgroup , CInt
#field entrynode , CInt
#field exitnode , CInt
#field nodeflags , CInt
#field nextseq , CInt
#stoptype
{- typedef struct {
            vec3_t org; int start; int end;
        } mstudiopivot_t; -}
#starttype mstudiopivot_t
#field org , <vec3_t>
#field start , CInt
#field end , CInt
#stoptype
{- typedef struct {
            char name[32]; int type; int bone; vec3_t org; vec3_t vectors[3];
        } mstudioattachment_t; -}
#starttype mstudioattachment_t
#array_field name , CChar
#field type , CInt
#field bone , CInt
#field org , <vec3_t>
#array_field vectors , <vec3_t>
#stoptype
{- typedef struct {
            unsigned short offset[6];
        } mstudioanim_t; -}
#starttype mstudioanim_t
#array_field offset , CUShort
#stoptype
{- typedef union {
            char valid; char total; short value;
        } mstudioanimvalue_t; -}
#starttype mstudioanimvalue_t
#field valid , CChar
#field total , CChar
#field value , CShort
#stoptype
{- typedef struct {
            char name[64]; int nummodels; int base; int modelindex;
        } mstudiobodyparts_t; -}
#starttype mstudiobodyparts_t
#array_field name , CChar
#field nummodels , CInt
#field base , CInt
#field modelindex , CInt
#stoptype
{- typedef struct {
            char name[64]; int flags; int width; int height; int index;
        } mstudiotexture_t; -}
#starttype mstudiotexture_t
#array_field name , CChar
#field flags , CInt
#field width , CInt
#field height , CInt
#field index , CInt
#stoptype
{- typedef struct {
            char name[64];
            int type;
            float boundingradius;
            int nummesh;
            int meshindex;
            int numverts;
            int vertinfoindex;
            int vertindex;
            int numnorms;
            int norminfoindex;
            int normindex;
            int numgroups;
            int groupindex;
        } mstudiomodel_t; -}
#starttype mstudiomodel_t
#array_field name , CChar
#field type , CInt
#field boundingradius , CFloat
#field nummesh , CInt
#field meshindex , CInt
#field numverts , CInt
#field vertinfoindex , CInt
#field vertindex , CInt
#field numnorms , CInt
#field norminfoindex , CInt
#field normindex , CInt
#field numgroups , CInt
#field groupindex , CInt
#stoptype
{- typedef struct {
            int numtris;
            int triindex;
            int skinref;
            int numnorms;
            int normindex;
        } mstudiomesh_t; -}
#starttype mstudiomesh_t
#field numtris , CInt
#field triindex , CInt
#field skinref , CInt
#field numnorms , CInt
#field normindex , CInt
#stoptype
{- typedef struct {
            short vertindex; short normindex; short s, t;
        } mstudiotrivert_t; -}
#starttype mstudiotrivert_t
#field vertindex , CShort
#field normindex , CShort
#field s , CShort
#field t , CShort
#stoptype
