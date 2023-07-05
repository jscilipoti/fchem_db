module substance
  use json_module
  use constants, only: pr
  implicit none
  
  type :: property
    character(len=:), allocatable :: name
    character(len=:), allocatable :: units
    real(pr) :: value
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
    type(property) :: Solubility_parameter
    type(property) :: Dipole_moment
    type(property) :: IG_heat_of_formation
    type(property) :: IG_Gibbs_energy_of_formation
    type(property) :: IG_absolute_entropy
    type(property) :: Heat_of_fusion_at_melting_point
    type(property) :: Standard_net_heat_of_combustion_LHV
    type(property) :: Liquid_density
    type(property) :: Vapour_pressure
    type(property) :: Heat_of_vaporization
    type(property) :: Liquid_heat_capacity
    type(property) :: Ideal_gas_heat_capacity
    type(property) :: Second_virial_coefficient
    type(property) :: Liquid_viscosity
    type(property) :: Vapour_viscosity
    type(property) :: Liquid_thermal_conductivity
    type(property) :: Vapour_thermal_conductivity
    type(property) :: Ideal_gas_heat_capacity__RPP
    type(property) :: Relative_static_permittivity
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
    type(property) :: Chao_Seader_acentric_factor
    type(property) :: Chao_Seader_solubility_parameter
    type(property) :: Chao_Seader_liquid_volume
    type(property) :: CAS_number
  
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

    if (self%search(name)) then
      ! initialize the class
      call json%initialize()  
  
      ! read the file
      call json%load(filename = 'lecture/db_json/'//name//'.json')
      
      ! read properties
      call json%get('LibraryIndex.value', self%Index%value, found)
      call json%get('CompoundID.value', self%Name%value, found)
      call json%get('StructureFormula.value', self%Structure%value, found)
      call json%get('Family.value', self%Family%value, found)
      call json%get('CriticalTemperature.value', self%Critical_temperature%value, found)
      call json%get('CriticalPressure.value', self%Critical_pressure%value, found)
      call json%get('CriticalVolume.value', self%Critical_volume%value, found)
      call json%get('CriticalCompressibility.value', self%Critical_compressibility_factor%value, found)
      call json%get('NormalBoilingPointTemperature.value', self%Normal_boiling_point%value, found)
      call json%get('NormalMeltingPointTemperature.value', self%Melting_point%value, found)
      call json%get('TriplePointTemperature.value', self%Triple_point_temperature%value, found)
      call json%get('TriplePointPressure.value', self%Triple_point_pressure%value, found)
      call json%get('MolecularWeight.value', self%Molecular_weight%value, found)
      call json%get('LiquidVolumeAtNormalBoilingPoint.value', self%Liquid_molar_volume_at_normal_boiling_point%value, found)
      call json%get('AcentricityFactor.value', self%Acentric_factor%value, found)
      call json%get('SolubilityParameter.value', self%Solubility_parameter%value, found)
      call json%get('DipoleMoment.value', self%Dipole_moment%value, found)
      call json%get('HeatOfFormation.value', self%IG_heat_of_formation%value, found)
      call json%get('GibbsEnergyOfFormation.value', self%IG_Gibbs_energy_of_formation%value, found)
      call json%get('AbsEntropy.value', self%IG_absolute_entropy%value, found)
      call json%get('HeatOfFusionAtMeltingPoint.value', self%Heat_of_fusion_at_melting_point%value, found)
      call json%get('HeatOfCombustion.value', self%Standard_net_heat_of_combustion_LHV%value, found)
      call json%get('LiquidDensity.value', self%Liquid_density%value, found)
      call json%get('VaporPressure.value', self%Vapour_pressure%value, found)
      call json%get('HeatOfVaporization.value', self%Heat_of_vaporization%value, found)
      call json%get('LiquidHeatCapacityCp.value', self%Liquid_heat_capacity%value, found)
      call json%get('IdealGasHeatCapacityCp.value', self%Ideal_gas_heat_capacity%value, found)
      call json%get('SecondVirialCoefficient.value', self%Second_virial_coefficient%value, found)
      call json%get('LiquidViscosity.value', self%Liquid_viscosity%value, found)
      call json%get('VaporViscosity.value', self%Vapour_viscosity%value, found)
      call json%get('LiquidThermalConductivity.value', self%Liquid_thermal_conductivity%value, found)
      call json%get('VaporThermalConductivity.value', self%Vapour_thermal_conductivity%value, found)
      call json%get('RPPHeatCapacityCp.value', self%Ideal_gas_heat_capacity__RPP%value, found)
      call json%get('RelativeStaticPermittivity.value', self%Relative_static_permittivity%value, found)
      call json%get('AntoineVaporPressure.value', self%Antoine%value, found)
      call json%get('LiquidViscosityRPS.value', self%Liquid_viscosity__RPS%value, found)
      call json%get('COSTALDVolume.value', self%COSTLD_characteristic_volume__V%value, found)
      call json%get('DiameterLJ.value', self%Lennard_Jones_diameter%value, found)
      call json%get('EnergyLJ.value', self%Lennard_Jones_energy%value, found)
      call json%get('RacketParameter.value', self%Rackett_parameter%value, found)
      call json%get('FullerVolume.value', self%Fuller_diffusion_volume%value, found)
      call json%get('Parachor.value', self%Parachor%value, found)
      call json%get('SpecificGravity.value', self%Specific_gravity%value, found)
      call json%get('Charge.value', self%Charge%value, found)
      call json%get('CostaldAcentricFactor.value', self%SRK_acentric_factor%value, found)
      call json%get('WilsonVolume.value', self%Wilson_volume%value, found)
      call json%get('ChaoSeaderAcentricFactor.value', self%Chao_Seader_acentric_factor%value, found)
      call json%get('ChaoSeaderSolubilityParameter.value', self%Chao_Seader_solubility_parameter%value, found)
      call json%get('ChaoSeaderLiquidVolume.value', self%Chao_Seader_liquid_volume%value, found)
      call json%get('CAS.value', self%CAS_number%value, found)
            
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
    call system('ls ./lecture/db_json > fileContents.txt')
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
