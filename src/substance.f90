module substance
  use json_module
  use properties_names
  use constants, only: pr
  implicit none
  
  type :: property
    character(len=:), allocatable :: name
    character(len=:), allocatable :: units
    character(len=250), allocatable :: value_str(:)
    real(pr),allocatable :: value(:)
  end type property
  
  type :: substances
  type(property) :: Index
  type(property) :: Name
  type(property) :: Structure
  type(property) :: Family
  type(property) :: Critical_temperature
  type(property) :: Critical_pressure
  type(property) :: Critical_volume
  type(property) :: Critical_compressibility_factor
  type(property) :: Normal_boiling_point
  type(property) :: Melting_point
  type(property) :: Triple_point_temperature
  type(property) :: Triple_point_pressure
  type(property) :: Molecular_weight
  type(property) :: Liquid_molar_volume_at_normal_boiling_point
  type(property) :: Acentric_factor
  type(property) :: Radius_of_gyration
  type(property) :: Solubility_parameter
  type(property) :: Dipole_moment
  type(property) :: Van_der_Waals_volume
  type(property) :: Van_der_Waals_area
  type(property) :: IG_heat_of_formation
  type(property) :: IG_Gibbs_energy_of_formation
  type(property) :: IG_absolute_entropy
  type(property) :: Heat_of_fusion_at_melting_point
  type(property) :: Standard_net_heat_of_combustion_LHV
  type(property) :: Solid_density
  type(property) :: Liquid_density
  type(property) :: Vapour_pressure
  type(property) :: Heat_of_vaporization
  type(property) :: Solid_heat_capacity
  type(property) :: Liquid_heat_capacity
  type(property) :: Ideal_gas_heat_capacity
  type(property) :: Second_virial_coefficient
  type(property) :: Liquid_viscosity
  type(property) :: Vapour_viscosity
  type(property) :: Liquid_thermal_conductivity
  type(property) :: Vapour_thermal_conductivity
  type(property) :: Surface_tension
  type(property) :: Ideal_gas_heat_capacity__RPP
  type(property) :: Antoine
  type(property) :: Liquid_viscosity__RPS
  type(property) :: COSTLD_characteristic_volume__V
  type(property) :: Lennard_Jones_diameter
  type(property) :: Lennard_Jones_energy
  type(property) :: Rackett_parameter
  type(property) :: Fuller_diffusion_volume
  type(property) :: Parachor
  type(property) :: Specific_gravity
  type(property) :: Charge
  type(property) :: SRK_acentric_factor
  type(property) :: Wilson_volume
  type(property) :: UNIQUAC_r
  type(property) :: UNIQUAC_q
  type(property) :: Chao_Seader_acentric_factor
  type(property) :: Chao_Seader_solubility_parameter
  type(property) :: Chao_Seader_liquid_volume
  type(property) :: UNIFAC
  type(property) :: UMR
  type(property) :: CAS_number
  type(property) :: SMILES
  
  contains 
    procedure :: read => read_db
    procedure, private :: search => search_compound
  end type substances

contains

  subroutine read_db(self, name)
    class(substances), intent(inout) :: self
    character(len=*), intent(in) :: name
    
    type(json_file) :: json
    logical :: found
    real(pr) :: MW
    type (PropertiesNames) :: names(properties_number)
    type (PropertiesNames) :: names_ChemSep(properties_number)

    if (self%search(name)) then
      
      ! initializate arrays of properties names
      names = arrays_of_properties_names()
      names_ChemSep = arrays_of_properties_names_ChemSep()
      
      ! initialize the class 
      call json%initialize()  

      ! read the file
      call json%load(filename = 'files/db_json/'//name//'.json')
      
      ! read properties

      call json%get(names_ChemSep(1)%s//'.name', self%Index%name, found)
      call json%get(names_ChemSep(1)%s//'.units', self%Index%units, found)
      call json%get(names_ChemSep(1)%s//'.value', self%Index%value, found)
      call json%get(names_ChemSep(2)%s//'.name', self%Name%name, found)
      call json%get(names_ChemSep(2)%s//'.units', self%Name%units, found)
      call json%get(names_ChemSep(2)%s//'.value', self%Name%value_str, found)
      call json%get(names_ChemSep(3)%s//'.name', self%Structure%name, found)
      call json%get(names_ChemSep(3)%s//'.units', self%Structure%units, found)
      call json%get(names_ChemSep(3)%s//'.value', self%Structure%value_str, found)
      call json%get(names_ChemSep(4)%s//'.name', self%Family%name, found)
      call json%get(names_ChemSep(4)%s//'.units', self%Family%units, found)
      call json%get(names_ChemSep(4)%s//'.value', self%Family%value, found)
      call json%get(names_ChemSep(5)%s//'.name', self%Critical_temperature%name, found)
      call json%get(names_ChemSep(5)%s//'.units', self%Critical_temperature%units, found)
      call json%get(names_ChemSep(5)%s//'.value', self%Critical_temperature%value, found)
      call json%get(names_ChemSep(6)%s//'.name', self%Critical_pressure%name, found)
      call json%get(names_ChemSep(6)%s//'.units', self%Critical_pressure%units, found)
      call json%get(names_ChemSep(6)%s//'.value', self%Critical_pressure%value, found)
      call json%get(names_ChemSep(7)%s//'.name', self%Critical_volume%name, found)
      call json%get(names_ChemSep(7)%s//'.units', self%Critical_volume%units, found)
      call json%get(names_ChemSep(7)%s//'.value', self%Critical_volume%value, found)
      call json%get(names_ChemSep(8)%s//'.name', self%Critical_compressibility_factor%name, found)
      call json%get(names_ChemSep(8)%s//'.units', self%Critical_compressibility_factor%units, found)
      call json%get(names_ChemSep(8)%s//'.value', self%Critical_compressibility_factor%value, found)
      call json%get(names_ChemSep(9)%s//'.name', self%Normal_boiling_point%name, found)
      call json%get(names_ChemSep(9)%s//'.units', self%Normal_boiling_point%units, found)
      call json%get(names_ChemSep(9)%s//'.value', self%Normal_boiling_point%value, found)
      call json%get(names_ChemSep(10)%s//'.name', self%Melting_point%name, found)
      call json%get(names_ChemSep(10)%s//'.units', self%Melting_point%units, found)
      call json%get(names_ChemSep(10)%s//'.value', self%Melting_point%value, found)
      call json%get(names_ChemSep(11)%s//'.name', self%Triple_point_temperature%name, found)
      call json%get(names_ChemSep(11)%s//'.units', self%Triple_point_temperature%units, found)
      call json%get(names_ChemSep(11)%s//'.value', self%Triple_point_temperature%value, found)
      call json%get(names_ChemSep(12)%s//'.name', self%Triple_point_pressure%name, found)
      call json%get(names_ChemSep(12)%s//'.units', self%Triple_point_pressure%units, found)
      call json%get(names_ChemSep(12)%s//'.value', self%Triple_point_pressure%value, found)
      call json%get(names_ChemSep(13)%s//'.name', self%Molecular_weight%name, found)
      call json%get(names_ChemSep(13)%s//'.units', self%Molecular_weight%units, found)
      call json%get(names_ChemSep(13)%s//'.value', self%Molecular_weight%value, found)
      call json%get(names_ChemSep(14)%s//'.name', self%Liquid_molar_volume_at_normal_boiling_point%name, found)
      call json%get(names_ChemSep(14)%s//'.units', self%Liquid_molar_volume_at_normal_boiling_point%units, found)
      call json%get(names_ChemSep(14)%s//'.value', self%Liquid_molar_volume_at_normal_boiling_point%value, found)
      call json%get(names_ChemSep(15)%s//'.name', self%Acentric_factor%name, found)
      call json%get(names_ChemSep(15)%s//'.units', self%Acentric_factor%units, found)
      call json%get(names_ChemSep(15)%s//'.value', self%Acentric_factor%value, found)
      call json%get(names_ChemSep(16)%s//'.name', self%Radius_of_gyration%name, found)
      call json%get(names_ChemSep(16)%s//'.units', self%Radius_of_gyration%units, found)
      call json%get(names_ChemSep(16)%s//'.value', self%Radius_of_gyration%value, found)
      call json%get(names_ChemSep(17)%s//'.name', self%Solubility_parameter%name, found)
      call json%get(names_ChemSep(17)%s//'.units', self%Solubility_parameter%units, found)
      call json%get(names_ChemSep(17)%s//'.value', self%Solubility_parameter%value, found)
      call json%get(names_ChemSep(18)%s//'.name', self%Dipole_moment%name, found)
      call json%get(names_ChemSep(18)%s//'.units', self%Dipole_moment%units, found)
      call json%get(names_ChemSep(18)%s//'.value', self%Dipole_moment%value, found)
      call json%get(names_ChemSep(19)%s//'.name', self%Van_der_Waals_volume%name, found)
      call json%get(names_ChemSep(19)%s//'.units', self%Van_der_Waals_volume%units, found)
      call json%get(names_ChemSep(19)%s//'.value', self%Van_der_Waals_volume%value, found)
      call json%get(names_ChemSep(20)%s//'.name', self%Van_der_Waals_area%name, found)
      call json%get(names_ChemSep(20)%s//'.units', self%Van_der_Waals_area%units, found)
      call json%get(names_ChemSep(20)%s//'.value', self%Van_der_Waals_area%value, found)
      call json%get(names_ChemSep(21)%s//'.name', self%IG_heat_of_formation%name, found)
      call json%get(names_ChemSep(21)%s//'.units', self%IG_heat_of_formation%units, found)
      call json%get(names_ChemSep(21)%s//'.value', self%IG_heat_of_formation%value, found)
      call json%get(names_ChemSep(22)%s//'.name', self%IG_Gibbs_energy_of_formation%name, found)
      call json%get(names_ChemSep(22)%s//'.units', self%IG_Gibbs_energy_of_formation%units, found)
      call json%get(names_ChemSep(22)%s//'.value', self%IG_Gibbs_energy_of_formation%value, found)
      call json%get(names_ChemSep(23)%s//'.name', self%IG_absolute_entropy%name, found)
      call json%get(names_ChemSep(23)%s//'.units', self%IG_absolute_entropy%units, found)
      call json%get(names_ChemSep(23)%s//'.value', self%IG_absolute_entropy%value, found)
      call json%get(names_ChemSep(24)%s//'.name', self%Heat_of_fusion_at_melting_point%name, found)
      call json%get(names_ChemSep(24)%s//'.units', self%Heat_of_fusion_at_melting_point%units, found)
      call json%get(names_ChemSep(24)%s//'.value', self%Heat_of_fusion_at_melting_point%value, found)
      call json%get(names_ChemSep(25)%s//'.name', self%Standard_net_heat_of_combustion_LHV%name, found)
      call json%get(names_ChemSep(25)%s//'.units', self%Standard_net_heat_of_combustion_LHV%units, found)
      call json%get(names_ChemSep(25)%s//'.value', self%Standard_net_heat_of_combustion_LHV%value, found)
      call json%get(names_ChemSep(26)%s//'.name', self%Solid_density%name, found)
      call json%get(names_ChemSep(26)%s//'.units', self%Solid_density%units, found)
      call json%get(names_ChemSep(27)%s//'.name', self%Liquid_density%name, found)
      call json%get(names_ChemSep(27)%s//'.units', self%Liquid_density%units, found)
      call json%get(names_ChemSep(28)%s//'.name', self%Vapour_pressure%name, found)
      call json%get(names_ChemSep(28)%s//'.units', self%Vapour_pressure%units, found)
      call json%get(names_ChemSep(29)%s//'.name', self%Heat_of_vaporization%name, found)
      call json%get(names_ChemSep(29)%s//'.units', self%Heat_of_vaporization%units, found)
      call json%get(names_ChemSep(30)%s//'.name', self%Solid_heat_capacity%name, found)
      call json%get(names_ChemSep(30)%s//'.units', self%Solid_heat_capacity%units, found)
      call json%get(names_ChemSep(31)%s//'.name', self%Liquid_heat_capacity%name, found)
      call json%get(names_ChemSep(31)%s//'.units', self%Liquid_heat_capacity%units, found)
      call json%get(names_ChemSep(32)%s//'.name', self%Ideal_gas_heat_capacity%name, found)
      call json%get(names_ChemSep(32)%s//'.units', self%Ideal_gas_heat_capacity%units, found)
      call json%get(names_ChemSep(33)%s//'.name', self%Second_virial_coefficient%name, found)
      call json%get(names_ChemSep(33)%s//'.units', self%Second_virial_coefficient%units, found)
      call json%get(names_ChemSep(34)%s//'.name', self%Liquid_viscosity%name, found)
      call json%get(names_ChemSep(34)%s//'.units', self%Liquid_viscosity%units, found)
      call json%get(names_ChemSep(35)%s//'.name', self%Vapour_viscosity%name, found)
      call json%get(names_ChemSep(35)%s//'.units', self%Vapour_viscosity%units, found)
      call json%get(names_ChemSep(36)%s//'.name', self%Liquid_thermal_conductivity%name, found)
      call json%get(names_ChemSep(36)%s//'.units', self%Liquid_thermal_conductivity%units, found)
      call json%get(names_ChemSep(37)%s//'.name', self%Vapour_thermal_conductivity%name, found)
      call json%get(names_ChemSep(37)%s//'.units', self%Vapour_thermal_conductivity%units, found)
      call json%get(names_ChemSep(38)%s//'.name', self%Surface_tension%name, found)
      call json%get(names_ChemSep(38)%s//'.units', self%Surface_tension%units, found)
      call json%get(names_ChemSep(39)%s//'.name', self%Ideal_gas_heat_capacity__RPP%name, found)
      call json%get(names_ChemSep(39)%s//'.units', self%Ideal_gas_heat_capacity__RPP%units, found)
      call json%get(names_ChemSep(40)%s//'.name', self%Antoine%name, found)
      call json%get(names_ChemSep(40)%s//'.units', self%Antoine%units, found)
      call json%get(names_ChemSep(41)%s//'.name', self%Liquid_viscosity__RPS%name, found)
      call json%get(names_ChemSep(41)%s//'.units', self%Liquid_viscosity__RPS%units, found)
      call json%get(names_ChemSep(42)%s//'.name', self%COSTLD_characteristic_volume__V%name, found)
      call json%get(names_ChemSep(42)%s//'.units', self%COSTLD_characteristic_volume__V%units, found)
      call json%get(names_ChemSep(42)%s//'.value', self%COSTLD_characteristic_volume__V%value, found)
      call json%get(names_ChemSep(43)%s//'.name', self%Lennard_Jones_diameter%name, found)
      call json%get(names_ChemSep(43)%s//'.units', self%Lennard_Jones_diameter%units, found)
      call json%get(names_ChemSep(43)%s//'.value', self%Lennard_Jones_diameter%value, found)
      call json%get(names_ChemSep(44)%s//'.name', self%Lennard_Jones_energy%name, found)
      call json%get(names_ChemSep(44)%s//'.units', self%Lennard_Jones_energy%units, found)
      call json%get(names_ChemSep(44)%s//'.value', self%Lennard_Jones_energy%value, found)
      call json%get(names_ChemSep(45)%s//'.name', self%Rackett_parameter%name, found)
      call json%get(names_ChemSep(45)%s//'.units', self%Rackett_parameter%units, found)
      call json%get(names_ChemSep(45)%s//'.value', self%Rackett_parameter%value, found)
      call json%get(names_ChemSep(46)%s//'.name', self%Fuller_diffusion_volume%name, found)
      call json%get(names_ChemSep(46)%s//'.units', self%Fuller_diffusion_volume%units, found)
      call json%get(names_ChemSep(46)%s//'.value', self%Fuller_diffusion_volume%value, found)
      call json%get(names_ChemSep(47)%s//'.name', self%Parachor%name, found)
      call json%get(names_ChemSep(47)%s//'.units', self%Parachor%units, found)
      call json%get(names_ChemSep(47)%s//'.value', self%Parachor%value, found)
      call json%get(names_ChemSep(48)%s//'.name', self%Specific_gravity%name, found)
      call json%get(names_ChemSep(48)%s//'.units', self%Specific_gravity%units, found)
      call json%get(names_ChemSep(48)%s//'.value', self%Specific_gravity%value, found)
      call json%get(names_ChemSep(49)%s//'.name', self%Charge%name, found)
      call json%get(names_ChemSep(49)%s//'.units', self%Charge%units, found)
      call json%get(names_ChemSep(49)%s//'.value', self%Charge%value, found)
      call json%get(names_ChemSep(50)%s//'.name', self%SRK_acentric_factor%name, found)
      call json%get(names_ChemSep(50)%s//'.units', self%SRK_acentric_factor%units, found)
      call json%get(names_ChemSep(50)%s//'.value', self%SRK_acentric_factor%value, found)
      call json%get(names_ChemSep(51)%s//'.name', self%Wilson_volume%name, found)
      call json%get(names_ChemSep(51)%s//'.units', self%Wilson_volume%units, found)
      call json%get(names_ChemSep(51)%s//'.value', self%Wilson_volume%value, found)
      call json%get(names_ChemSep(52)%s//'.name', self%UNIQUAC_r%name, found)
      call json%get(names_ChemSep(52)%s//'.units', self%UNIQUAC_r%units, found)
      call json%get(names_ChemSep(52)%s//'.value', self%UNIQUAC_r%value, found)
      call json%get(names_ChemSep(53)%s//'.name', self%UNIQUAC_q%name, found)
      call json%get(names_ChemSep(53)%s//'.units', self%UNIQUAC_q%units, found)
      call json%get(names_ChemSep(53)%s//'.value', self%UNIQUAC_q%value, found)
      call json%get(names_ChemSep(54)%s//'.name', self%Chao_Seader_acentric_factor%name, found)
      call json%get(names_ChemSep(54)%s//'.units', self%Chao_Seader_acentric_factor%units, found)
      call json%get(names_ChemSep(54)%s//'.value', self%Chao_Seader_acentric_factor%value, found)
      call json%get(names_ChemSep(55)%s//'.name', self%Chao_Seader_solubility_parameter%name, found)
      call json%get(names_ChemSep(55)%s//'.units', self%Chao_Seader_solubility_parameter%units, found)
      call json%get(names_ChemSep(55)%s//'.value', self%Chao_Seader_solubility_parameter%value, found)
      call json%get(names_ChemSep(56)%s//'.name', self%Chao_Seader_liquid_volume%name, found)
      call json%get(names_ChemSep(56)%s//'.units', self%Chao_Seader_liquid_volume%units, found)
      call json%get(names_ChemSep(56)%s//'.value', self%Chao_Seader_liquid_volume%value, found)
      call json%get(names_ChemSep(57)%s//'.name', self%UNIFAC%name, found)
      call json%get(names_ChemSep(57)%s//'.units', self%UNIFAC%units, found)
      call json%get(names_ChemSep(58)%s//'.name', self%UMR%name, found)
      call json%get(names_ChemSep(58)%s//'.units', self%UMR%units, found)
      call json%get(names_ChemSep(59)%s//'.name', self%CAS_number%name, found)
      call json%get(names_ChemSep(59)%s//'.units', self%CAS_number%units, found)
      call json%get(names_ChemSep(59)%s//'.value', self%CAS_number%value_str, found)
      call json%get(names_ChemSep(60)%s//'.name', self%SMILES%name, found)
      call json%get(names_ChemSep(60)%s//'.units', self%SMILES%units, found)
      call json%get(names_ChemSep(60)%s//'.value', self%SMILES%value_str, found)
    else 

      write(*,*) "Compound not found"

    end if
  end subroutine read_db

  logical function search_compound(self,name) result(found)
    class(substances), intent(in) :: self
    character(len=*), intent(in) :: name
    real :: r
    integer :: i,reason,NstationFiles,iStation
    character(250) :: stationFileNames, nameint
    !logical :: found

    nameint = name//".json"
    ! get the files
    call system('ls ./files/db_json > fileContents.txt')
    open(31,FILE='fileContents.txt',action="read")
    !how many
    i = 0
    do
      read(31,FMT='(a)',iostat=reason) r
      if (reason/=0) EXIT
      i = i+1
    end do
    
    NstationFiles = i

    rewind(31)
    found = .false.
    do i = 1,NstationFiles
      read(31,'(a)') stationFileNames
      if (stationFileNames == nameint) then
        found = .true.
        return
      end if
    enddo
    
  end function search_compound  
end module substance
