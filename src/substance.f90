module substance
  use json_module
  use constants, only: pr
  implicit none
  
  type :: substances
    real(pr) :: Index
    real(pr) :: Name
    real(pr) :: Structure
    real(pr) :: Family
    real(pr) :: Critical_temperature
    real(pr) :: Critical_pressure
    real(pr) :: Critical_volume
    real(pr) :: Critical_compressibility_factor
    real(pr) :: Normal_boiling_point
    real(pr) :: Melting_point
    real(pr) :: Triple_point_temperature
    real(pr) :: Triple_point_pressure
    real(pr) :: Molecular_weight
    real(pr) :: Liquid_molar_volume_at_normal_boiling_point
    real(pr) :: Acentric_factor
    real(pr) :: Solubility_parameter
    real(pr) :: Dipole_moment
    real(pr) :: IG_heat_of_formation
    real(pr) :: IG_Gibbs_energy_of_formation
    real(pr) :: IG_absolute_entropy
    real(pr) :: Heat_of_fusion_at_melting_point
    real(pr) :: Standard_net_heat_of_combustion_LHV
    real(pr) :: Liquid_density
    real(pr) :: Vapour_pressure
    real(pr) :: Heat_of_vaporization
    real(pr) :: Liquid_heat_capacity
    real(pr) :: Ideal_gas_heat_capacity
    real(pr) :: Second_virial_coefficient
    real(pr) :: Liquid_viscosity
    real(pr) :: Vapour_viscosity
    real(pr) :: Liquid_thermal_conductivity
    real(pr) :: Vapour_thermal_conductivity
    real(pr) :: Ideal_gas_heat_capacity__RPP
    real(pr) :: Relative_static_permittivity
    real(pr) :: Antoine
    real(pr) :: Liquid_viscosity__RPS
    real(pr) :: COSTLD_characteristic_volume__V
    real(pr) :: Lennard_Jones_diameter
    real(pr) :: Lennard_Jones_energy
    real(pr) :: Rackett_parameter
    real(pr) :: Fuller_diffusion_volume
    real(pr) :: Parachor
    real(pr) :: Specific_gravity
    real(pr) :: Charge
    real(pr) :: SRK_acentric_factor
    real(pr) :: Wilson_volume
    real(pr) :: Chao_Seader_acentric_factor
    real(pr) :: Chao_Seader_solubility_parameter
    real(pr) :: Chao_Seader_liquid_volume
    real(pr) :: CAS_number
  
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
      call json%get('LibraryIndex.value', self%Index, found)
      call json%get('CompoundID.value', self%Name, found)
      call json%get('StructureFormula.value', self%Structure, found)
      call json%get('Family.value', self%Family, found)
      call json%get('CriticalTemperature.value', self%Critical_temperature, found)
      call json%get('CriticalPressure.value', self%Critical_pressure, found)
      call json%get('CriticalVolume.value', self%Critical_volume, found)
      call json%get('CriticalCompressibility.value', self%Critical_compressibility_factor, found)
      call json%get('NormalBoilingPointTemperature.value', self%Normal_boiling_point, found)
      call json%get('NormalMeltingPointTemperature.value', self%Melting_point, found)
      call json%get('TriplePointTemperature.value', self%Triple_point_temperature, found)
      call json%get('TriplePointPressure.value', self%Triple_point_pressure, found)
      call json%get('MolecularWeight.value', self%Molecular_weight, found)
      call json%get('LiquidVolumeAtNormalBoilingPoint.value', self%Liquid_molar_volume_at_normal_boiling_point, found)
      call json%get('AcentricityFactor.value', self%Acentric_factor, found)
      call json%get('SolubilityParameter.value', self%Solubility_parameter, found)
      call json%get('DipoleMoment.value', self%Dipole_moment, found)
      call json%get('HeatOfFormation.value', self%IG_heat_of_formation, found)
      call json%get('GibbsEnergyOfFormation.value', self%IG_Gibbs_energy_of_formation, found)
      call json%get('AbsEntropy.value', self%IG_absolute_entropy, found)
      call json%get('HeatOfFusionAtMeltingPoint.value', self%Heat_of_fusion_at_melting_point, found)
      call json%get('HeatOfCombustion.value', self%Standard_net_heat_of_combustion_LHV, found)
      call json%get('LiquidDensity.value', self%Liquid_density, found)
      call json%get('VaporPressure.value', self%Vapour_pressure, found)
      call json%get('HeatOfVaporization.value', self%Heat_of_vaporization, found)
      call json%get('LiquidHeatCapacityCp.value', self%Liquid_heat_capacity, found)
      call json%get('IdealGasHeatCapacityCp.value', self%Ideal_gas_heat_capacity, found)
      call json%get('SecondVirialCoefficient.value', self%Second_virial_coefficient, found)
      call json%get('LiquidViscosity.value', self%Liquid_viscosity, found)
      call json%get('VaporViscosity.value', self%Vapour_viscosity, found)
      call json%get('LiquidThermalConductivity.value', self%Liquid_thermal_conductivity, found)
      call json%get('VaporThermalConductivity.value', self%Vapour_thermal_conductivity, found)
      call json%get('RPPHeatCapacityCp.value', self%Ideal_gas_heat_capacity__RPP, found)
      call json%get('RelativeStaticPermittivity.value', self%Relative_static_permittivity, found)
      call json%get('AntoineVaporPressure.value', self%Antoine, found)
      call json%get('LiquidViscosityRPS.value', self%Liquid_viscosity__RPS, found)
      call json%get('COSTALDVolume.value', self%COSTLD_characteristic_volume__V, found)
      call json%get('DiameterLJ.value', self%Lennard_Jones_diameter, found)
      call json%get('EnergyLJ.value', self%Lennard_Jones_energy, found)
      call json%get('RacketParameter.value', self%Rackett_parameter, found)
      call json%get('FullerVolume.value', self%Fuller_diffusion_volume, found)
      call json%get('Parachor.value', self%Parachor, found)
      call json%get('SpecificGravity.value', self%Specific_gravity, found)
      call json%get('Charge.value', self%Charge, found)
      call json%get('CostaldAcentricFactor.value', self%SRK_acentric_factor, found)
      call json%get('WilsonVolume.value', self%Wilson_volume, found)
      call json%get('ChaoSeaderAcentricFactor.value', self%Chao_Seader_acentric_factor, found)
      call json%get('ChaoSeaderSolubilityParameter.value', self%Chao_Seader_solubility_parameter, found)
      call json%get('ChaoSeaderLiquidVolume.value', self%Chao_Seader_liquid_volume, found)
      call json%get('CAS.value', self%CAS_number, found)
      
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
