module substance
  use json_module
  use properties_names
  use constants, only: pr
  implicit none
  
  type :: GroupType
    character(len=10) :: id
    character(len=10) :: value    
  end type GroupType

  type :: Property
    character(len=:), allocatable :: name
    character(len=:), allocatable :: units
    character(len=250), allocatable :: value_str(:) !when value is a string data type
    real(pr),allocatable :: value(:)                !when value is a real data type
    real(pr) :: A, B, C, D, E, Tmin, Tmax
    integer :: eqno
    character(len=10), allocatable :: group(:)

  end type Property
  
  type :: Substances
    type(Property) :: Index
    type(Property) :: Name
    type(Property) :: Structure
    type(Property) :: Family
    type(Property) :: Critical_temperature
    type(Property) :: Critical_pressure
    type(Property) :: Critical_volume
    type(Property) :: Critical_compressibility_factor
    type(Property) :: Normal_boiling_point
    type(Property) :: Melting_point
    type(Property) :: Triple_point_temperature
    type(Property) :: Triple_point_pressure
    type(Property) :: Molecular_weight
    type(Property) :: Liquid_molar_volume_at_normal_boiling_point
    type(Property) :: Acentric_factor
    type(Property) :: Radius_of_gyration
    type(Property) :: Solubility_parameter
    type(Property) :: Dipole_moment
    type(Property) :: Van_der_Waals_volume
    type(Property) :: Van_der_Waals_area
    type(Property) :: IG_heat_of_formation
    type(Property) :: IG_Gibbs_energy_of_formation
    type(Property) :: IG_absolute_entropy
    type(Property) :: Heat_of_fusion_at_melting_point
    type(Property) :: Standard_net_heat_of_combustion_LHV
    type(Property) :: Solid_density
    type(Property) :: Liquid_density
    type(Property) :: Vapour_pressure
    type(Property) :: Heat_of_vaporization
    type(Property) :: Solid_heat_capacity
    type(Property) :: Liquid_heat_capacity
    type(Property) :: Ideal_gas_heat_capacity
    type(Property) :: Second_virial_coefficient
    type(Property) :: Liquid_viscosity
    type(Property) :: Vapour_viscosity
    type(Property) :: Liquid_thermal_conductivity
    type(Property) :: Vapour_thermal_conductivity
    type(Property) :: Surface_tension
    type(Property) :: Ideal_gas_heat_capacity__RPP
    type(Property) :: Antoine
    type(Property) :: Liquid_viscosity__RPS
    type(Property) :: COSTLD_characteristic_volume__V
    type(Property) :: Lennard_Jones_diameter
    type(Property) :: Lennard_Jones_energy
    type(Property) :: Rackett_parameter
    type(Property) :: Fuller_diffusion_volume
    type(Property) :: Parachor
    type(Property) :: Specific_gravity
    type(Property) :: Charge
    type(Property) :: SRK_acentric_factor
    type(Property) :: Wilson_volume
    type(Property) :: UNIQUAC_r
    type(Property) :: UNIQUAC_q
    type(Property) :: Chao_Seader_acentric_factor
    type(Property) :: Chao_Seader_solubility_parameter
    type(Property) :: Chao_Seader_liquid_volume
    type(Property) :: UNIFAC
    type(Property) :: UMR
    type(Property) :: CAS_number
    type(Property) :: SMILES
  
  contains 
    procedure :: read => read_db
    procedure, private :: search => search_compound
  end type substances

contains

  subroutine read_db(self, name)

    use constants, only: database_dir

    implicit none

    class(substances), intent(inout) :: self
    character(len=*), intent(in) :: name 
    character(len=:), allocatable :: temp
    character(len=10) :: MyString
    integer :: i
    
    type(json_file) :: json
    logical :: found
 !   real(pr) :: MW
    type (PropertiesNames) :: names(properties_number)
    type (PropertiesNames) :: names_ChemSep(properties_number)

    
    if (self%search(name)) then !Search if the compound is in the database
      
      ! initializate arrays of properties names
      names = arrays_of_properties_names()
      names_ChemSep = arrays_of_properties_names_ChemSep()
      
      ! initialize the class 
      call json%initialize()  

      ! read the file
      if (allocated(database_dir)) then
        print *, database_dir//name//'.json'
        call json%load(filename = database_dir//name//'.json')
      else
        call json%load(filename = 'build/dependencies/fchem_db/files/db_json/'//name//'.json')
      end if
      
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
      call json%get(names_ChemSep(26)%s//'.eqno.value', self%Solid_density%eqno, found)
      call json%get(names_ChemSep(26)%s//'.A.value', self%Solid_density%A, found)
      call json%get(names_ChemSep(26)%s//'.B.value', self%Solid_density%B, found)
      call json%get(names_ChemSep(26)%s//'.Tmin.value', self%Solid_density%Tmin, found)
      call json%get(names_ChemSep(26)%s//'.Tmax.value', self%Solid_density%Tmax, found)
      
      call json%get(names_ChemSep(27)%s//'.name', self%Liquid_density%name, found)
      call json%get(names_ChemSep(27)%s//'.units', self%Liquid_density%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Liquid_density%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Liquid_density%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Liquid_density%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Liquid_density%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Liquid_density%D, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Liquid_density%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Liquid_density%Tmax, found)
      
      call json%get(names_ChemSep(28)%s//'.name', self%Vapour_pressure%name, found)
      call json%get(names_ChemSep(28)%s//'.units', self%Vapour_pressure%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Vapour_pressure%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Vapour_pressure%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Vapour_pressure%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Vapour_pressure%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Vapour_pressure%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Vapour_pressure%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Vapour_pressure%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Vapour_pressure%Tmax, found)
      
      call json%get(names_ChemSep(29)%s//'.name', self%Heat_of_vaporization%name, found)
      call json%get(names_ChemSep(29)%s//'.units', self%Heat_of_vaporization%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Heat_of_vaporization%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Heat_of_vaporization%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Heat_of_vaporization%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Heat_of_vaporization%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Heat_of_vaporization%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Heat_of_vaporization%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Heat_of_vaporization%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Heat_of_vaporization%Tmax, found)    

      call json%get(names_ChemSep(30)%s//'.name', self%Solid_heat_capacity%name, found)
      call json%get(names_ChemSep(30)%s//'.units', self%Solid_heat_capacity%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Solid_heat_capacity%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Solid_heat_capacity%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Solid_heat_capacity%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Solid_heat_capacity%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Solid_heat_capacity%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Solid_heat_capacity%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Solid_heat_capacity%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Solid_heat_capacity%Tmax, found)    

      call json%get(names_ChemSep(31)%s//'.name', self%Liquid_heat_capacity%name, found)
      call json%get(names_ChemSep(31)%s//'.units', self%Liquid_heat_capacity%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Liquid_heat_capacity%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Liquid_heat_capacity%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Liquid_heat_capacity%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Liquid_heat_capacity%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Liquid_heat_capacity%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Liquid_heat_capacity%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Liquid_heat_capacity%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Liquid_heat_capacity%Tmax, found)          

      call json%get(names_ChemSep(32)%s//'.name', self%Ideal_gas_heat_capacity%name, found)
      call json%get(names_ChemSep(32)%s//'.units', self%Ideal_gas_heat_capacity%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Ideal_gas_heat_capacity%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Ideal_gas_heat_capacity%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Ideal_gas_heat_capacity%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Ideal_gas_heat_capacity%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Ideal_gas_heat_capacity%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Ideal_gas_heat_capacity%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Ideal_gas_heat_capacity%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Ideal_gas_heat_capacity%Tmax, found)          

      call json%get(names_ChemSep(33)%s//'.name', self%Second_virial_coefficient%name, found)
      call json%get(names_ChemSep(33)%s//'.units', self%Second_virial_coefficient%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Second_virial_coefficient%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Second_virial_coefficient%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Second_virial_coefficient%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Second_virial_coefficient%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Second_virial_coefficient%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Second_virial_coefficient%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Second_virial_coefficient%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Second_virial_coefficient%Tmax, found)      

      call json%get(names_ChemSep(34)%s//'.name', self%Liquid_viscosity%name, found)
      call json%get(names_ChemSep(34)%s//'.units', self%Liquid_viscosity%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Liquid_viscosity%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Liquid_viscosity%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Liquid_viscosity%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Liquid_viscosity%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Liquid_viscosity%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Liquid_viscosity%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Liquid_viscosity%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Liquid_viscosity%Tmax, found)      

      call json%get(names_ChemSep(35)%s//'.name', self%Vapour_viscosity%name, found)
      call json%get(names_ChemSep(35)%s//'.units', self%Vapour_viscosity%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Vapour_viscosity%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Vapour_viscosity%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Vapour_viscosity%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Vapour_viscosity%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Vapour_viscosity%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Vapour_viscosity%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Vapour_viscosity%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Vapour_viscosity%Tmax, found)        

      call json%get(names_ChemSep(36)%s//'.name', self%Liquid_thermal_conductivity%name, found)
      call json%get(names_ChemSep(36)%s//'.units', self%Liquid_thermal_conductivity%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Liquid_thermal_conductivity%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Liquid_thermal_conductivity%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Liquid_thermal_conductivity%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Liquid_thermal_conductivity%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Liquid_thermal_conductivity%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Liquid_thermal_conductivity%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Liquid_thermal_conductivity%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Liquid_thermal_conductivity%Tmax, found)  

      call json%get(names_ChemSep(37)%s//'.name', self%Vapour_thermal_conductivity%name, found)
      call json%get(names_ChemSep(37)%s//'.units', self%Vapour_thermal_conductivity%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Vapour_thermal_conductivity%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Vapour_thermal_conductivity%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Vapour_thermal_conductivity%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Vapour_thermal_conductivity%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Vapour_thermal_conductivity%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Vapour_thermal_conductivity%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Vapour_thermal_conductivity%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Vapour_thermal_conductivity%Tmax, found)  

      call json%get(names_ChemSep(38)%s//'.name', self%Surface_tension%name, found)
      call json%get(names_ChemSep(38)%s//'.units', self%Surface_tension%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Surface_tension%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Surface_tension%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Surface_tension%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Surface_tension%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Surface_tension%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Surface_tension%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Surface_tension%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Surface_tension%Tmax, found)  

      call json%get(names_ChemSep(39)%s//'.name', self%Ideal_gas_heat_capacity__RPP%name, found)
      call json%get(names_ChemSep(39)%s//'.units', self%Ideal_gas_heat_capacity__RPP%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Ideal_gas_heat_capacity__RPP%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Ideal_gas_heat_capacity__RPP%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Ideal_gas_heat_capacity__RPP%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Ideal_gas_heat_capacity__RPP%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Ideal_gas_heat_capacity__RPP%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Ideal_gas_heat_capacity__RPP%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Ideal_gas_heat_capacity__RPP%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Ideal_gas_heat_capacity__RPP%Tmax, found)  

      call json%get(names_ChemSep(40)%s//'.name', self%Antoine%name, found)
      call json%get(names_ChemSep(40)%s//'.units', self%Antoine%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Antoine%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Antoine%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Antoine%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Antoine%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Antoine%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Antoine%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Antoine%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Antoine%Tmax, found)  

      call json%get(names_ChemSep(41)%s//'.name', self%Liquid_viscosity__RPS%name, found)
      call json%get(names_ChemSep(41)%s//'.units', self%Liquid_viscosity__RPS%units, found)
      call json%get(names_ChemSep(27)%s//'.eqno.value', self%Liquid_viscosity__RPS%eqno, found)
      call json%get(names_ChemSep(27)%s//'.A.value', self%Liquid_viscosity__RPS%A, found)
      call json%get(names_ChemSep(27)%s//'.B.value', self%Liquid_viscosity__RPS%B, found)
      call json%get(names_ChemSep(27)%s//'.C.value', self%Liquid_viscosity__RPS%C, found)
      call json%get(names_ChemSep(27)%s//'.D.value', self%Liquid_viscosity__RPS%D, found)
      call json%get(names_ChemSep(27)%s//'.E.value', self%Liquid_viscosity__RPS%E, found)
      call json%get(names_ChemSep(27)%s//'.Tmin.value', self%Liquid_viscosity__RPS%Tmin, found)
      call json%get(names_ChemSep(27)%s//'.Tmax.value', self%Liquid_viscosity__RPS%Tmax, found)  

      call json%get(names_ChemSep(42)%s//'.name', self%COSTLD_characteristic_volume__V%name, found)
      call json%get(names_ChemSep(42)%s//'.units', self%COSTLD_characteristic_volume__V%units, found)
      call json%get(names_ChemSep(42)%s//'.value', self%COSTLD_characteristic_volume__V%value, found)
      
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
      i = 1
      do 
        write(MyString,'(i10)') i
        call json%get(names_ChemSep(57)%s//'.group('//trim(MyString)//').id', temp, found)
        print *, temp
        if (.not. found) exit
        i = i + 1
      end do
      
      print *, self%UNIFAC%group
     ! call json%get(names_ChemSep(57)%s//'.group', self%UNIFAC%group, found)

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

    use constants, only: database_dir

    implicit none     

    class(substances), intent(in) :: self
    character(len=*), intent(in) :: name
    real :: r
    integer :: i,reason,NstationFiles,iStation
    character(250) :: stationFileNames, nameint
    !logical :: found

    nameint = name//".json"
    ! get the files

    if (allocated(database_dir)) then
      call system('ls ./files/db_json > fileContents.txt')
    else
      call system('ls build/dependencies/fchem_db/files/db_json > fileContents.txt')
    end if

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
