!****************************************************************************
!
!    MODULE  var_suma.mod
!
!  PURPOSE:  Provides the common variables for sumafiles code
!
!   version 0.1  28 abril 2018
!
!***************************************************************************
module var_suma
!integer hh            ! Identifies the start time
integer :: zlev       ! Layer of emission (1 to 8) 8 lower 1 upper
integer :: radm,nradm   ! number of Mechanism classes
integer :: grid_id
integer,parameter::mozart=45 ! number of emision species in Mozart
integer,parameter::nh=24 ! number of hours per day
integer,parameter::nDims=6 ! number of dimensions in wrfchemi file
integer:: julyr,julday,mapproj,iswater,islake,isice,isurban,isoilwater
integer:: land_cat_stag
integer,allocatable::id_var(:)

real :: cenlat,cenlon, dx,dy
real :: trulat1, trulat2,moadcenlat,stdlon,pollat,pollon
real :: gmt,num_land_cat

real,allocatable ::xlon(:,:,:),xlat(:,:,:)! by nx,ny,nh emissions
real,allocatable ::vus(:,:,:,:) ! by nx,ny,nh,tim emissions array
real,allocatable:: EMI_USA(:,:,:,:,:) ! Emissions nx,ny,nh,tim,nradm
real,allocatable:: EMI_MEX(:,:,:,:,:) ! Emissions nx,ny,nh,tim,nradm

character(len=19)::mminlu,map_proj_char
character (len=38) :: Title
character (len=19) :: current_date,mecha
character (len=19),dimension(1,1)::Times ! Date character
character (len=11),allocatable :: ENAME(:),ENMX(:) !US and Mex emissions Names
character (len=11),dimension(mozart) :: EMOZ=(/ & !Mozart Emissions Names
'E_BENZENE','E_BIGALK','E_BIGENE','E_BZALD','E_C10H16','E_C2H2','E_C2H4',&
'E_C2H5OH','E_C2H6','E_C3H6','E_C3H8','E_CH2O','E_CH3CHO','E_CH3COCH3',&
'E_CH3OH','E_CH4','E_CO','E_CO2','E_CRESOL','E_TOLUENE','E_XYLENE',&
'E_GLY','E_HCOOH','E_ISOP','E_MACR','E_MEK','E_MGLY','E_MVK',&
'E_NH3','E_NO','E_NO2','E_PHENOL','E_SO2','E_NO3I','E_NO3J',&
'E_ORGI','E_ORGJ','E_PM_10','E_PM25','E_PM25I','E_PM25J','E_SO4I',&
'E_SO4J','E_ECI','E_ECJ'/)
character(len=19),dimension(mozart):: cname=(/& !Mozart Emissions description
  'Benzene ','lumped alkanes 1 C>3','lumped alkenes C>3','Benzaldehyde ',&
  'Alpha-pinene ','Ethyne ','Ethene ','Ethanol ','Ethane ','Propene ',&
  'Propane ','Formaldehyde ','Acetaldehyde ','Acetone ','Methanol ',&
  'Methane ','Carbon monoxide ','Carbon dioxide ','Cresol ',&
  'Toluene ','M-/o-/p-xylenes','Glyoxal ','Formic acid ',&
  'Isoprene ','Methacrolein ','Methyl ethyl ketone ','methyl glyoxal ',&
  'Methyl vinyl ketone ','Ammonia ','Ntrogen oxide ','Nitrogen dioxide ',&
  'Phenol ','Sulfur Dioxide ','Nitrates I ','Nitrates J ',&
  'Organic I ','Organic J ','PM10 ','PM2.5 ','PM2.5 I ','PM2.5 J ',&
  'Sulfates I ','Sulfates J ','Elemental C I ','ELemental C J '/)
character (len=19),dimension(NDIMS) ::sdim=(/"Time               ",&
& "DateStrLen         ","west_east          ","south_north        ",&
&"bottom_top         ","emissions_zdim_stag"/)

common /domain/ zlev,cenlat,cenlon,dx,dy,trulat1,&
               trulat2,moadcenlat,stdlon,pollat,pollon,Title
common /wrfchem/ julyr,julday,mapproj,iswater,islake,isice,isurban,isoilwater,&
                 gmt,num_land_cat,land_cat_stag,mminlu,map_proj_char,&
                 current_date,mecha,EMOZ,cname
contains
!
!  CCCC  H   H  EEEEE   CCCC  K   K
! CC     H   H  E      CC     K K
! C      HHHHH  EEE   C       KK
! CC     H   H  E      CC     K K
!  CCCC  H   H  EEEEE   CCCC  K   K
subroutine check(status)
USE netcdf
  integer, intent ( in) :: status
  if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    stop 2
  end if
end subroutine check

pure function a_mayuscula(palabra) result (cadena)
!   ++++++++++++++++++++++++++++++
!       Cambia a mayusculas
!   ++++++++++++++++++++++++++++++
implicit none
Character(*), Intent(In) :: palabra
Character(LEN(palabra))  :: cadena

Integer :: j, im

Character(26), Parameter :: minus = 'aeioubcdfghjklmnpqrstvwxyz'
Character(26), Parameter :: may   = 'AEIOUBCDFGHJKLMNPQRSTVWXYZ'

!   Cambia cada letra a mayuscula si es minuscula
  cadena = palabra
  do j = 1, len_trim(palabra)
    im = index(minus, palabra(j:j))
    if (im > 0) cadena(j:j) = may(im:im)
  end do
end function a_mayuscula

end module var_suma
