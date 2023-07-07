module properties_names
    use constants

    type PropertiesNames
        character(len=:), allocatable :: s
    end type PropertiesNames

contains

    function arrays_of_properties_names_ChemSep() result(names)
        implicit none
        type (PropertiesNames) :: names(properties_number)

        names(1)%s = 'LibraryIndex'
        names(2)%s = 'CompoundID'
        names(3)%s = 'StructureFormula'
        names(4)%s = 'Family'
        names(5)%s = 'CriticalTemperature'
        names(6)%s = 'CriticalPressure'
        names(7)%s = 'CriticalVolume'
        names(8)%s = 'CriticalCompressibility'
        names(9)%s = 'NormalBoilingPointTemperature'
        names(10)%s = 'NormalMeltingPointTemperature'
        names(11)%s = 'TriplePointTemperature'
        names(12)%s = 'TriplePointPressure'
        names(13)%s = 'MolecularWeight'
        names(14)%s = 'LiquidVolumeAtNormalBoilingPoint'
        names(15)%s = 'AcentricityFactor'
        names(16)%s = 'RadiusOfGyration'
        names(17)%s = 'SolubilityParameter'
        names(18)%s = 'DipoleMoment'
        names(19)%s = 'VanDerWaalsVolume'
        names(20)%s = 'VanDerWaalsArea'
        names(21)%s = 'HeatOfFormation'
        names(22)%s = 'GibbsEnergyOfFormation'
        names(23)%s = 'AbsEntropy'
        names(24)%s = 'HeatOfFusionAtMeltingPoint'
        names(25)%s = 'HeatOfCombustion'
        names(26)%s = 'SolidDensity'
        names(27)%s = 'LiquidDensity'
        names(28)%s = 'VaporPressure'
        names(29)%s = 'HeatOfVaporization'
        names(30)%s = 'SolidHeatCapacityCp'
        names(31)%s = 'LiquidHeatCapacityCp'
        names(32)%s = 'IdealGasHeatCapacityCp'
        names(33)%s = 'SecondVirialCoefficient'
        names(34)%s = 'LiquidViscosity'
        names(35)%s = 'VaporViscosity'
        names(36)%s = 'LiquidThermalConductivity'
        names(37)%s = 'VaporThermalConductivity'
        names(38)%s = 'SurfaceTension'
        names(39)%s = 'RPPHeatCapacityCp'
        names(40)%s = 'AntoineVaporPressure'
        names(41)%s = 'LiquidViscosityRPS'
        names(42)%s = 'COSTALDVolume'
        names(43)%s = 'DiameterLJ'
        names(44)%s = 'EnergyLJ'
        names(45)%s = 'RacketParameter'
        names(46)%s = 'FullerVolume'
        names(47)%s = 'Parachor'
        names(48)%s = 'SpecificGravity'
        names(49)%s = 'Charge'
        names(50)%s = 'CostaldAcentricFactor'
        names(51)%s = 'WilsonVolume'
        names(52)%s = 'UniquacR'
        names(53)%s = 'UniquacQ'
        names(54)%s = 'ChaoSeaderAcentricFactor'
        names(55)%s = 'ChaoSeaderSolubilityParameter'
        names(56)%s = 'ChaoSeaderLiquidVolume'
        names(57)%s = 'UnifacVLE'
        names(58)%s = 'Umr'
        names(59)%s = 'CAS'
        names(60)%s = 'Smiles'
        


    end function arrays_of_properties_names_ChemSep

    function arrays_of_properties_names() result(names)
        implicit none
        type (PropertiesNames) :: names(properties_number)

        names(1)%s = 'Index'
        names(2)%s = 'Name'
        names(3)%s = 'Structure'
        names(4)%s = 'Family'
        names(5)%s = 'Critical_temperature'
        names(6)%s = 'Critical_pressure'
        names(7)%s = 'Critical_volume'
        names(8)%s = 'Critical_compressibility_factor'
        names(9)%s = 'Normal_boiling_point'
        names(10)%s = 'Melting_point'
        names(11)%s = 'Triple_point_temperature'
        names(12)%s = 'Triple_point_pressure'
        names(13)%s = 'Molecular_weight'
        names(14)%s = 'Liquid_molar_volume_at_normal_boiling_point'
        names(15)%s = 'Acentric_factor'
        names(16)%s = 'Radius_of_gyration'
        names(17)%s = 'Solubility_parameter'
        names(18)%s = 'Dipole_moment'
        names(19)%s = 'Van_der_Waals_volume'
        names(20)%s = 'Van_der_Waals_area'
        names(21)%s = 'IG_heat_of_formation'
        names(22)%s = 'IG_Gibbs_energy_of_formation'
        names(23)%s = 'IG_absolute_entropy'
        names(24)%s = 'Heat_of_fusion_at_melting_point'
        names(25)%s = 'Standard_net_heat_of_combustion_LHV'
        names(26)%s = 'Solid_density'
        names(27)%s = 'Liquid_density'
        names(28)%s = 'Vapour_pressure'
        names(29)%s = 'Heat_of_vaporization'
        names(30)%s = 'Solid_heat_capacity'
        names(31)%s = 'Liquid_heat_capacity'
        names(32)%s = 'Ideal_gas_heat_capacity'
        names(33)%s = 'Second_virial_coefficient'
        names(34)%s = 'Liquid_viscosity'
        names(35)%s = 'Vapour_viscosity'
        names(36)%s = 'Liquid_thermal_conductivity'
        names(37)%s = 'Vapour_thermal_conductivity'
        names(38)%s = 'Surface_tension'
        names(39)%s = 'Ideal_gas_heat_capacity__RPP'
        names(40)%s = 'Antoine'
        names(41)%s = 'Liquid_viscosity__RPS'
        names(42)%s = 'COSTLD_characteristic_volume__V'
        names(43)%s = 'Lennard_Jones_diameter'
        names(44)%s = 'Lennard_Jones_energy'
        names(45)%s = 'Rackett_parameter'
        names(46)%s = 'Fuller_diffusion_volume'
        names(47)%s = 'Parachor'
        names(48)%s = 'Specific_gravity'
        names(49)%s = 'Charge'
        names(50)%s = 'SRK_acentric_factor'
        names(51)%s = 'Wilson_volume'
        names(52)%s = 'UNIQUAC_r'
        names(53)%s = 'UNIQUAC_q'
        names(54)%s = 'Chao_Seader_acentric_factor'
        names(55)%s = 'Chao_Seader_solubility_parameter'
        names(56)%s = 'Chao_Seader_liquid_volume'
        names(57)%s = 'UNIFAC'
        names(58)%s = 'UMR'
        names(59)%s = 'CAS_number'
        names(60)%s = 'SMILES'
        


    end function arrays_of_properties_names

end module properties_names