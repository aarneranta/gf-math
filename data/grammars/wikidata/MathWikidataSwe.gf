concrete MathWikidataSwe of MathWikidata = 
open SyntaxSwe, ParadigmsSwe in {
lincat QN = CN ;

oper mkQN = overload {
  mkQN : CN -> CN = \n -> n ;  -- for working around the heuristics

  mkQN : (_ : Str) -> CN = \x -> mkCN (mkN x) ;
  
  mkQN : (_, _ : Str) -> CN = \x, y ->
    case x of {
      adj + "t" => mkCN (mkA adj) (mkN y neutrum) ;
      adj => mkCN (mkA x) (mkN y utrum)
      } ;
    
--  mkQN : (_, _, _ : Str) -> QN = mkCN ;
--  mkQN : (_, _, _, _ : Str) -> QN = mkCN ;
--  mkQN : (_, _, _, _, _ : Str) -> QN = mkCN ;
  } ;

{-
lin essential_supremum_Q1969054_QN = mkQN "Väsentligt" "supremum" "och" "väsentligt" "infimum" ;
lin algebra_over_a_field_Q1000660_QN = mkQN "Algebra" "över" "en" "kropp" ;
lin eigenvectors_and_eigenvalues_Q190524_QN = mkQN "Egenvärde," "egenvektor" "och" "egenrum" ;
lin Carl_Gustav_Jacob_Jacobi_Q76564_QN = mkQN "Carl" "Gustav" "Jacob" "Jacobi" ;
lin law_of_large_numbers_Q207952_QN = mkQN "de" "stora" "talens" "lag" ;
lin greatest_common_divisor_Q131752_QN = mkQN "största" "gemensamma" "delare" ;
lin nowhere_dense_set_Q1991405_QN = mkQN "Ingenstans" "tät" "mängd" ;
lin quotient_space_Q1393796_QN = mkQN "Kvotrum" "(linjär" "algebra)" ;
lin signed_measure_Q1764371_QN = mkQN "mått" "med" "tecken" ;
lin simply_connected_space_Q912058_QN = mkQN "Enkelt" "sammanhängande" "mängd" ;
lin universal_enveloping_algebra_Q1673406_QN = mkQN "universell" "envelopperande" "algebra" ;
lin least_common_multiple_Q102761_QN = mkQN "minsta" "gemensamma" "multipel" ;
-}
lin abelian_group_Q181296_QN = mkQN "abelsk" "grupp" ;
lin affine_transformation_Q382497_QN = mkQN "affin" "avbildning" ;
lin almost_everywhere_Q1139334_QN = mkQN "Nästan" "överallt" ;
lin alternating_group_Q438814_QN = mkQN "Alternerande" "grupp" ;
lin barycentric_coordinate_system_Q738422_QN = mkQN "Barycentriska" "koordinater" ;
lin bijection_Q180907_QN = mkQN "bijektiv" "funktion" ;
lin binary_operation_Q164307_QN = mkQN "binär" "operator" ;
lin binary_relation_Q130901_QN = mkQN "binär" "relation" ;
lin bounded_set_Q726212_QN = mkQN "Begränsad" "mängd" ;
lin complex_unit_circle_Q105356493_QN = mkQN "komplex" "enhetscirkel" ;
lin closed_set_Q320357_QN = mkQN "Sluten" "mängd" ;
lin closure_Q320346_QN = mkQN "Slutet" "hölje" ;
lin Hermitian_conjugate_matrix_Q2051983_QN = mkQN "Hermiteskt" "konjugat" ;
lin connected_space_Q1491995_QN = mkQN "Sammanhängande" "rum" ;
lin continuous_function_Q170058_QN = mkQN "kontinuerlig" "funktion" ;
lin convex_set_Q193657_QN = mkQN "konvex" "mängd" ;
lin coprime_Q104752_QN = mkQN "relativt" "prima" ;
lin critical_point_Q577705_QN = mkQN "Kritisk" "punkt" ;
lin cyclic_permutation_Q212130_QN = mkQN "cyklisk" "permutation" ;
lin cyclic_group_Q245462_QN = mkQN "cyklisk" "grupp" ;
lin dense_set_Q673444_QN = mkQN "tät" "mängd" ;
lin elementary_matrix_Q609566_QN = mkQN "Elementär" "matris" ;
lin analytic_function_Q215084_QN = mkQN "analytisk" "funktion" ;
lin Euclidean_domain_Q867345_QN = mkQN "Euklidiskt" "område" ;
lin Euler's_totient_function_Q190026_QN = mkQN "Eulers" "fi-funktion" ;
lin exact_sequence_Q1326955_QN = mkQN "exakt" "följd" ;
lin exterior_product_Q13408581_QN = mkQN "Yttre" "produkt" ;
lin finitely_generated_module_Q1340572_QN = mkQN "Ändligtgenererad" "modul" ;
lin free_module_Q1292333_QN = mkQN "Fri" "modul" ;
lin Gaussian_integer_Q724975_QN = mkQN "Gaussiska" "heltal" ;
lin harmonic_function_Q599027_QN = mkQN "Harmonisk" "funktion" ;
lin injection_Q182003_QN = mkQN "injektiv" "funktion" ;
lin inner_automorphism_Q1139583_QN = mkQN "Inre" "automorfi" ;
lin inner_product_Q23924662_QN = mkQN "Inre" "produktrum" ;
lin inner_product_space_Q214159_QN = mkQN "Inre" "produktrum" ;
lin interior_Q862761_QN = mkQN "det" "inre" ;
lin inverse_element_Q338057_QN = mkQN "inverst" "element" ;
lin inverse_function_Q191884_QN = mkQN "invers" "funktion" ;
lin irreducible_element_Q2989575_QN = mkQN "Irreducibelt" "element" ;
lin measurable_function_Q516776_QN = mkQN "mätbar" "funktion" ;
lin identity_element_Q185813_QN = mkQN "neutralt" "element" ;
lin linear_map_Q207643_QN = mkQN "linjär" "avbildning" ;
lin linear_independence_Q27670_QN = mkQN "Linjärt" "oberoende" ;
lin metric_space_Q180953_QN = mkQN "Metriskt" "rum" ;
lin natural_transformation_Q1442189_QN = mkQN "naturlig" "transformation" ;
lin Noetherian_module_Q2444982_QN = mkQN "Noethersk" "modul" ;
lin noetherian_ring_Q582271_QN = mkQN "Noethersk" "ring" ;
lin normal_subgroup_Q743179_QN = mkQN "Normal" "delgrupp" ;
lin open_set_Q213363_QN = mkQN "Öppen" "mängd" ;
lin generating_function_Q860609_QN = mkQN "Genererande" "funktion" ;
lin directed_graph_Q1137726_QN = mkQN "riktad" "graf" ;
lin Orthogonal_complement_Q1780921_QN = mkQN "Ortogonalt" "komplement" ;
lin outer_measure_Q258374_QN = mkQN "Yttre" "mått" ;
lin partial_derivative_Q186475_QN = mkQN "Partiell" "derivata" ;
lin 'positive-definite_matrix_Q1052034_QN' = mkQN "Matrisers" "teckenkaraktär" ;
lin 'Radon–Nikodym_theorem_Q1191319_QN' = mkQN "Radon-Nikodyms" "sats" ;
lin reflexive_relation_Q621850_QN = mkQN "reflexiv" "relation" ;
lin semisimple_module_Q1485102_QN = mkQN "halvenkel" "modul" ;
lin separable_space_Q680081_QN = mkQN "Separabelt" "rum" ;
lin simple_function_Q913022_QN = mkQN "enkel" "funktion" ;
lin simple_group_Q571124_QN = mkQN "enkel" "grupp" ;
lin simple_module_Q956233_QN = mkQN "enkel" "modul" ;
lin singular_homology_Q1095056_QN = mkQN "singulär" "homologi" ;
lin Singular_measure_Q1824450_QN = mkQN "Singulärt" "mått" ;
lin small_category_Q10567583_QN = mkQN "Liten" "kategori" ;
lin smooth_function_Q868473_QN = mkQN "glatt" "funktion" ;
lin solvable_group_Q759832_QN = mkQN "lösbar" "grupp" ;
lin linear_span_Q209812_QN = mkQN "Linjärt" "hölje" ;
lin surjective_function_Q229102_QN = mkQN "surjektiv" "funktion" ;
lin suspension_Q1307987_QN = mkQN "Suspension" "(matematik)" ;
lin symmetric_relation_Q621461_QN = mkQN "symmetrisk" "relation" ;
lin symmetric_difference_Q1147242_QN = mkQN "symmetrisk" "differens" ;
lin symmetric_group_Q849512_QN = mkQN "symmetrisk" "grupp" ;
lin topological_group_Q1046291_QN = mkQN "topologisk" "grupp" ;
lin topological_space_Q179899_QN = mkQN "topologiskt" "rum" ;
lin transitive_relation_Q64861_QN = mkQN "transitiv" "relation" ;
lin uniform_continuity_Q741865_QN = mkQN "Likformig" "kontinuitet" ;
lin unitary_matrix_Q727103_QN = mkQN "Unitär" "matris" ;
lin vector_space_Q125977_QN = mkQN "linjärt" "rum" ;
lin weak_Hausdorff_space_Q7977932_QN = mkQN "Svagt" "Hausdorffrum" ;
lin alternating_series_Q438842_QN = mkQN "alternerande" "serie" ;
lin antiderivative_Q114326_QN = mkQN "primitiv" "funktion" ;
lin 'Bolzano–Weierstrass_theorem_Q468391_QN' = mkQN "Bolzano-Weierstrass" "sats" ;
lin 'Borel–Cantelli_lemma_Q893496_QN' = mkQN "Borel-Cantellis" "lemma" ;
lin Bézout's_identity_Q513028_QN = mkQN "Bézouts" "identitet" ;
lin Cartesian_product_Q173740_QN = mkQN "cartesisk" "produkt" ;
lin 'Cauchy–Riemann_equations_Q622741_QN' = mkQN "Cauchy–Riemanns" "ekvationer" ;
lin 'Cauchy–Schwarz_inequality_Q190546_QN' = mkQN "Cauchy-Schwarz" "olikhet" ;
lin 'Cayley–Hamilton_theorem_Q656772_QN' = mkQN "Cayley-Hamiltons" "sats" ;
lin characteristic_function_Q822139_QN = mkQN "karakteristisk" "funktion" ;
lin Chinese_remainder_theorem_Q193878_QN = mkQN "kinesiska" "restklassatsen" ;
lin commutative_ring_Q858656_QN = mkQN "Kommutativ" "ring" ;
lin Euler's_formula_Q184871_QN = mkQN "Eulers" "formel" ;
lin complex_number_Q11567_QN = mkQN "komplext" "tal" ;
lin conditional_probability_Q327069_QN = mkQN "betingad" "sannolikhet" ;
lin 'Banach_fixed-point_theorem_Q220680_QN' = mkQN "Banachs" "fixpunktssats" ;
lin countable_set_Q66707394_QN = mkQN "Uppräknelig" "mängd" ;
lin dominated_convergence_theorem_Q1067156_QN = mkQN "dominerade" "konvergenssatsen" ;
lin Eisenstein's_criterion_Q1057416_QN = mkQN "Eisensteins" "kriterium" ;
lin empty_set_Q226183_QN = mkQN "tomma" "mängden" ;
lin Euclidean_algorithm_Q230848_QN = mkQN "Euklides" "algoritm" ;
lin even_number_Q13366104_QN = mkQN "jämnt" "tal" ;
lin exponential_growth_Q582659_QN = mkQN "exponentiell" "tillväxt" ;
lin exterior_algebra_Q1196652_QN = mkQN "Yttre" "algebra" ;
lin Fast_Fourier_transform_Q623950_QN = mkQN "snabb" "fouriertransform" ;
lin Fatou's_lemma_Q1068118_QN = mkQN "Fatous" "lemma" ;
lin Fejér's_theorem_Q571154_QN = mkQN "Fejérs" "sats" ;
lin finite_field_Q603880_QN = mkQN "ändlig" "kropp" ;
lin finite_set_Q272404_QN = mkQN "Ändlig" "mängd" ;
lin fundamental_theorem_of_algebra_Q192760_QN = mkQN "algebrans" "fundamentalsats" ;
lin geometric_series_Q1306887_QN = mkQN "geometrisk" "summa" ;
lin 'Heine–Cantor_theorem_Q765987_QN' = mkQN "Heine-Cantors" "sats" ;
lin holomorphic_function_Q207476_QN = mkQN "analytisk" "funktion" ;
lin hyperbolic_function_Q204034_QN = mkQN "hyperbolisk" "funktion" ;
lin implicit_function_theorem_Q848375_QN = mkQN "implicita" "funktionssatsen" ;
lin improper_integral_Q464118_QN = mkQN "Generaliserad" "integral" ;
lin infinite_set_Q205140_QN = mkQN "oändlig" "mängd" ;
lin intermediate_value_theorem_Q245098_QN = mkQN "Bolzanos" "sats" ;
lin inverse_function_theorem_Q931001_QN = mkQN "inversa" "funktionssatsen" ;
lin invertible_matrix_Q242188_QN = mkQN "Inverterbar" "matris" ;
lin Jordan_normal_form_Q838495_QN = mkQN "Jordans" "normalform" ;
lin meromorphic_function_Q217616_QN = mkQN "meromorf" "funktion" ;
lin monotone_convergence_theorem_Q1153584_QN = mkQN "monotona" "konvergenssatsen" ;
lin monotonic_function_Q194404_QN = mkQN "monoton" "funktion" ;
lin Monte_Carlo_method_Q232207_QN = mkQN "Monte" "Carlo-metod" ;
lin natural_number_Q21199_QN = mkQN "naturligt" "tal" ; --- WD: naturliga tal
lin negative_real_number_Q200227_QN = mkQN "negativt" "tal" ; --- WD: negativa tal
lin noncommutative_ring_Q7049224_QN = mkQN "okommutativ" "ring" ;
lin normed_vector_space_Q726210_QN = mkQN "normerat" "rum" ;
lin odd_number_Q13366129_QN = mkQN "udda" "tal" ;
lin ordinary_differential_equation_Q465274_QN = mkQN "ordinär" "differentialekvation" ;
lin orthonormal_basis_Q2365325_QN = mkQN "ortonormerad" "bas" ;
lin periodic_function_Q184743_QN = mkQN "periodisk" "funktion" ;
lin positive_real_number_Q3176558_QN = mkQN "positiva" "tal" ;
lin mathematical_proof_Q11538_QN = mkQN "matematiskt" "bevis" ;
lin quadratic_form_Q736753_QN = mkQN "kvadratisk" "form" ;
lin rational_function_Q41237_QN = mkQN "rationell" "funktion" ;
lin rational_number_Q1244890_QN = mkQN "rationella" "tal" ;
lin real_number_Q12916_QN = mkQN "reella" "tal" ;
lin Riesz_representation_theorem_Q1357684_QN = mkQN "Riesz" "representationssats" ;
lin Rolle's_theorem_Q193286_QN = mkQN "Rolles" "sats" ;
lin Sylvester's_law_of_inertia_Q1752621_QN = mkQN "Sylvesters" "tröghetslag" ;
lin trigonometric_function_Q93344_QN = mkQN "trigonometrisk" "funktion" ;
lin uniform_convergence_Q1411887_QN = mkQN "Likformig" "konvergens" ;
lin Uniform_law_Q16862633_QN = mkQN "Uniform" "law" ;
lin abelianization_Q318598_QN = variants {} ;
lin absolute_continuity_Q332504_QN = variants {} ;
lin Absolutely_continuous_measures_Q20827138_QN = variants {} ;
lin absolute_value_Q120812_QN = mkQN "absolutbelopp" ;
lin transpose_of_a_linear_map_Q2858846_QN = variants {} ;
lin adjoint_representation_of_a_Lie_algebra_Q4379157_QN = variants {} ;
lin adjoint_representation_Q1017106_QN = variants {} ;
lin adjugate_matrix_Q225107_QN = variants {} ;
lin field_of_sets_Q246506_QN = variants {} ;
lin almost_uniform_convergence_Q25377974_QN = variants {} ;
lin annihilator_Q567083_QN = variants {} ;
lin associativity_Q177251_QN = mkQN "associativitet" ;
lin associative_algebra_Q744960_QN = variants {} ;
lin augmentation_ideal_Q4820423_QN = variants {} ;
lin automorphism_Q782566_QN = mkQN "Automorfi" ;
lin automorphism_group_Q60790315_QN = variants {} ;
lin Baire_class_Q2976036_QN = variants {} ;
lin 'Banach–Mazur_game_Q3459695_QN' = variants {} ;
lin Banach_space_Q194397_QN = mkQN "Banachrum" ;
lin Barycentric_subdivision_Q4078260_QN = variants {} ;
lin basis_Q810193_QN = variants {} ;
lin measurable_space_Q3058218_QN = variants {} ;
lin Borel_algebra_Q1080067_QN = mkQN "Borelalgebra" ;
lin boundary_Q875399_QN = mkQN "rand" ;
lin boundary_Q78323465_QN = variants {} ;
lin function_of_bounded_variation_Q829653_QN = variants {} ;
lin bundle_Q1047307_QN = variants {} ;
lin differentiable_manifold_Q3552958_QN = variants {} ;
lin Cantor_set_Q273188_QN = mkQN "Cantormängden" ;
lin category_Q719395_QN = mkQN "kategori" ;
lin category_of_sets_Q2518298_QN = variants {} ;
lin cauchy_sequence_Q217847_QN = mkQN "cauchyföljd" ;
lin cellular_homology_Q3139849_QN = variants {} ;
lin center_Q1195852_QN = mkQN "centrum" ;
lin centralizer_Q190629_QN = variants {} ;
lin centroid_Q511093_QN = variants {} ;
lin chain_complex_Q1143328_QN = mkQN "kedjekomplex" ;
lin character_table_Q11498021_QN = variants {} ;
lin characteristic_polynomial_Q849705_QN = variants {} ;
lin characteristic_subgroup_Q747027_QN = variants {} ;
lin chart_Q3828144_QN = variants {} ;
lin Choquet_game_Q18205969_QN = variants {} ;
lin differentiability_class_Q2138760_QN = variants {} ;
lin class_function_Q986909_QN = variants {} ;
lin loop_Q4558786_QN = variants {} ;
lin 'half-space_Q644719_QN' = variants {} ;
lin cocomplete_category_Q47007889_QN = variants {} ;
lin cofibration_Q1048949_QN = variants {} ;
lin commutative_property_Q165474_QN = mkQN "kommutativitet" ;
lin commutator_subgroup_Q522216_QN = variants {} ;
lin compact_space_Q381892_QN = mkQN "Kompakt" ;
lin compact_operator_Q1780743_QN = variants {} ;
lin compact_symplectic_group_Q78484790_QN = variants {} ;
lin compactly_generated_space_Q1738274_QN = variants {} ;
lin complete_category_Q4370335_QN = variants {} ;
lin complete_metric_space_Q848569_QN = variants {} ;
lin complex_conjugate_Q381040_QN = mkQN "Komplexkonjugat" ;
lin complex_Lie_group_Q5156554_QN = variants {} ;
lin complexification_Q445569_QN = variants {} ;
lin composition_series_Q2525646_QN = variants {} ;
lin cone_Q1635267_QN = variants {} ;
lin conjugacy_class_Q1353233_QN = variants {} ;
lin connected_category_Q5161408_QN = variants {} ;
lin connected_component_Q91050456_QN = variants {} ;
lin connected_graph_Q230655_QN = mkQN "komponent" ;
lin contractible_space_Q230689_QN = variants {} ;
lin 'set-theoretic_limit_Q1131116_QN' = variants {} ;
lin contraction_mapping_Q515173_QN = mkQN "kontraktionsavbildning" ;
lin convergence_in_measure_Q768656_QN = variants {} ;
lin covering_space_Q332648_QN = variants {} ;
lin critical_value_Q3754024_QN = variants {} ;
lin CW_complex_Q189061_QN = variants {} ;
lin retract_Q2141963_QN = variants {} ;
lin degree_of_a_polynomial_Q1473607_QN = mkQN "polynomgrad" ;
lin derivation_Q451323_QN = mkQN "Derivation" ;
lin lower_central_series_Q109314515_QN = variants {} ;
lin determinant_Q178546_QN = mkQN "determinant" ;
lin diffeomorphism_Q1058314_QN = mkQN "diffeomorfi" ;
lin total_derivative_Q636889_QN = variants {} ;
lin differential_form_Q1047080_QN = mkQN "differentialform" ;
lin dimension_of_a_vector_space_Q929302_QN = mkQN "dimension" ;
lin direct_sum_Q13582243_QN = variants {} ;
lin direct_sum_of_modules_Q1142861_QN = variants {} ;
lin directional_derivative_Q383851_QN = mkQN "Riktningsderivata" ;
lin discrete_category_Q1228851_QN = variants {} ;
lin discrete_topology_Q91267214_QN = variants {} ;
lin divisible_group_Q1782332_QN = variants {} ;
lin division_algebra_Q1231309_QN = variants {} ;
lin division_Q1226939_QN = mkQN "division" ;
lin division_ring_Q650741_QN = mkQN "Skevkropp" ;
lin dual_representation_Q3427408_QN = variants {} ;
lin dual_space_Q752487_QN = mkQN "Dualrum" ;
lin path_Q1415372_QN = mkQN "väg" ;
lin eigenvalue_Q3553768_QN = mkQN "Egenvärde" ;
lin eigenvector_Q3555174_QN = mkQN "egenvektor" ;
lin elementary_abelian_group_Q1017231_QN = variants {} ;
lin manifold_with_boundary_Q1333611_QN = variants {} ;
lin embedding_Q980509_QN = variants {} ;
lin endomorphism_Q1340800_QN = variants {} ;
lin epimorphism_Q1347208_QN = mkQN "epimorfi" ;
lin equivalence_class_Q1211071_QN = mkQN "ekvivalensklass" ;
lin equivalence_relation_Q130998_QN = mkQN "ekvivalensrelation" ;
lin equivariant_map_Q256355_QN = variants {} ;
lin scalar_product_Q181365_QN = mkQN "Skalärprodukt" ;
lin Euler_characteristic_Q852973_QN = mkQN "Eulerkarakteristik" ;
lin exponent_of_a_group_Q32998717_QN = variants {} ;
lin exterior_Q1322786_QN = variants {} ;
lin fiber_Q1640811_QN = variants {} ;
lin fiber_functor_Q16897605_QN = variants {} ;
lin fibration_Q493941_QN = variants {} ;
lin field_Q190109_QN = mkQN "kropp" ;
lin Filled_Julia_set_Q5448799_QN = variants {} ;
lin length_Q1761084_QN = variants {} ;
lin Frattini_subgroup_Q176244_QN = variants {} ;
lin free_group_Q431078_QN = variants {} ;
lin functional_Q579600_QN = mkQN "funktional" ;
lin functor_Q864475_QN = mkQN "funktor" ;
lin fundamental_group_Q662830_QN = mkQN "fundamentalgrupp" ;
lin fundamental_groupoid_Q85174068_QN = variants {} ;
lin Borel_determinacy_theorem_Q4944906_QN = variants {} ;
lin strategic_game_Q15079592_QN = variants {} ;
lin general_linear_Lie_algebra_Q17521172_QN = variants {} ;
lin general_linear_group_Q524607_QN = variants {} ;
lin generating_set_of_a_module_Q25106477_QN = variants {} ;
lin generating_set_of_a_group_Q734209_QN = variants {} ;
lin gradient_Q173582_QN = mkQN "gradient" ;
lin graph_Q141488_QN = mkQN "graf" ;
lin graph_of_a_function_Q182598_QN = mkQN "linjediagram" ;
lin mathematical_group_Q83478_QN = mkQN "grupp" ;
lin group_action_Q288465_QN = mkQN "gruppverkan" ;
lin fixed_point_Q217608_QN = mkQN "fixpunkt" ;
lin group_algebra_Q17019511_QN = variants {} ;
lin group_representation_Q1055807_QN = variants {} ;
lin group_ring_Q2602722_QN = variants {} ;
lin groupoid_Q1196038_QN = variants {} ;
lin basis_Q189569_QN = mkQN "bas" ;
lin Hausdorff_space_Q326908_QN = mkQN "Hausdorffrum" ;
lin Hermitian_inner_product_Q77583424_QN = variants {} ;
lin Hessian_matrix_Q620495_QN = mkQN "hessmatris" ;
lin Hilbert_space_Q190056_QN = mkQN "hilbertrum" ;
lin holomorph_of_a_group_Q3139500_QN = variants {} ;
lin homeomorphism_Q202906_QN = mkQN "homeomorfi" ;
lin homologous_Q12715503_QN = variants {} ;
lin Homology_Q1144780_QN = mkQN "homologi" ;
lin group_homomorphism_Q868169_QN = variants {} ;
lin homotopy_Q746083_QN = mkQN "homotopi" ;
lin homotopy_category_Q14775269_QN = variants {} ;
lin mapping_cone_Q306560_QN = variants {} ;
lin homotopy_equivalence_Q1626409_QN = variants {} ;
lin idempotent_element_Q2243424_QN = variants {} ;
lin identity_component_Q5988368_QN = variants {} ;
lin identity_matrix_Q193794_QN = mkQN "enhetsmatris" ;
lin image_Q860623_QN = variants {} ;
lin index_of_a_subgroup_Q1464168_QN = variants {} ;
lin inner_regular_measure_Q976375_QN = variants {} ;
lin integral_domain_Q628792_QN = mkQN "Integritetsområde" ;
lin interval_Q185148_QN = mkQN "intervall" ;
lin invariant_subspace_Q2706744_QN = variants {} ;
lin inverse_matrix_Q17123743_QN = variants {} ;
lin irreducible_representation_Q13690522_QN = variants {} ;
lin isolated_point_Q1142889_QN = variants {} ;
lin isometry_Q740207_QN = mkQN "isometri" ;
lin isomorphism_Q189112_QN = mkQN "isomorfi" ;
lin Isotypic_component_Q17098238_QN = variants {} ;
lin Jacobian_determinant_Q1474543_QN = variants {} ;
lin Jacobian_matrix_Q506041_QN = mkQN "Jacobimatris" ;
lin kernel_Q574844_QN = mkQN "nollrum" ;
lin Kronecker_delta_Q192826_QN = mkQN "Kroneckerdelta" ;
lin Laplace_operator_Q203484_QN = mkQN "Laplaceoperatorn" ;
lin Lebesgue_measure_Q827230_QN = mkQN "Lebesguemått" ;
lin Lebesgue_outer_measure_Q25447455_QN = variants {} ;
lin ideal_Q44649_QN = mkQN "Ideal" ;
lin Lie_algebra_Q664495_QN = mkQN "Liealgebra" ;
lin algebra_homomorphism_Q3882299_QN = variants {} ;
lin Lie_algebra_representation_Q1136782_QN = variants {} ;
lin limit_Q1322614_QN = variants {} ;
lin limit_point_Q858223_QN = mkQN "gränspunkt" ;
lin linear_Lie_algebra_Q16977077_QN = variants {} ;
lin linear_dependence_Q111783780_QN = variants {} ;
lin local_homology_Q4265744_QN = variants {} ;
lin local_maximum_Q91052280_QN = variants {} ;
lin local_minimum_Q91052291_QN = variants {} ;
lin locally_compact_group_Q2147620_QN = variants {} ;
lin locally_finite_collection_Q643300_QN = variants {} ;
lin locally_finite_measure_Q2136937_QN = variants {} ;
lin 'locally_path-connected_space_Q16671984_QN' = variants {} ;
lin Lorentz_group_Q1334417_QN = variants {} ;
lin Lp_space_Q305936_QN = mkQN "Lp-rum" ;
lin Lusin's_theorem_Q1361393_QN = variants {} ;
lin magma_Q679903_QN = mkQN "Magma" ;
lin Mandelbrot_set_Q257_QN = mkQN "Mandelbrotmängden" ;
lin manifold_Q203920_QN = mkQN "mångfald" ;
lin mapping_cylinder_Q1014869_QN = variants {} ;
lin matrix_Q44337_QN = mkQN "matris" ;
lin Lie_group_Q622679_QN = mkQN "Liegrupp" ;
lin logarithm_of_a_matrix_Q902017_QN = mkQN "Matrislogaritm" ;
lin matrix_multiplication_Q1049914_QN = variants {} ;
lin transpose_matrix_Q223683_QN = mkQN "Transponat" ;
lin maximal_ideal_Q1203540_QN = variants {} ;
lin maximal_subgroup_Q16908484_QN = variants {} ;
lin spanning_tree_Q831672_QN = variants {} ;
lin meagre_set_Q1747745_QN = variants {} ;
lin measurable_set_Q3054889_QN = variants {} ;
lin measure_space_Q3058212_QN = variants {} ;
lin null_set_Q1201815_QN = mkQN "nollmängd" ;
lin minor_Q1341061_QN = mkQN "Minor" ;
lin homomorphism_Q215111_QN = mkQN "homomorfi" ;
lin module_Q18848_QN = mkQN "modul" ;
lin monoid_Q208237_QN = mkQN "monoid" ;
lin monomorphism_Q1945067_QN = variants {} ;
lin multilinearity_Q98929882_QN = variants {} ;
lin multiplicative_character_Q17099189_QN = variants {} ;
lin chain_Q4504468_QN = variants {} ;
lin neighborhood_Q2478475_QN = mkQN "omgivning" ;
lin nilpotent_element_Q840023_QN = variants {} ;
lin nilpotent_group_Q1755242_QN = variants {} ;
lin nilpotent_Lie_algebra_Q15148889_QN = variants {} ;
lin ultrametric_space_Q1897429_QN = variants {} ;
lin residual_set_Q97623888_QN = variants {} ;
lin norm_Q956437_QN = mkQN "norm" ;
lin 'normal-form_game_Q1069099_QN' = variants {} ;
lin normalizer_Q1761121_QN = mkQN "normalisator" ;
lin kernel_Q2914509_QN = mkQN "Nollrum" ;
lin open_cover_Q41509136_QN = variants {} ;
lin simplex_Q331350_QN = mkQN "simplex" ;
lin opposite_category_Q7098616_QN = variants {} ;
lin orbit_of_a_group_action_Q17859776_QN = mkQN "bana" ;
lin group_order_Q18408315_QN = variants {} ;
lin order_of_a_group_element_Q54555759_QN = variants {} ;
lin orthogonality_Q215067_QN = mkQN "Ortogonalitet" ;
lin orthogonal_Lie_algebra_Q52088805_QN = variants {} ;
lin orthogonal_group_Q1783179_QN = mkQN "Ortogonalgrupp" ;
lin orthogonal_matrix_Q333871_QN = mkQN "Ortogonalmatris" ;
lin orthonormality_Q1411166_QN = variants {} ;
lin oscillation_Q170475_QN = mkQN "oscillation" ;
lin outer_regular_measure_Q26851733_QN = variants {} ;
lin orientability_Q2748415_QN = mkQN "Orienterbarhet" ;
lin 'p-adic_integer_Q11756524_QN' = variants {} ;
lin 'p-group_Q286972_QN' = variants {} ;
lin Paradoxical_set_Q7134470_QN = variants {} ;
lin partition_Q1082910_QN = mkQN "Heltalspartition" ;
lin partition_of_unity_Q191690_QN = variants {} ;
lin path_Q1366002_QN = variants {} ;
lin 'path-connected_space_Q3487687_QN' = variants {} ;
lin perfect_set_Q7168101_QN = variants {} ;
lin permutation_Q161519_QN = mkQN "Permutation" ;
lin polynomial_ring_Q1455652_QN = mkQN "Polynomring" ;
lin Pontryagin_duality_Q1632419_QN = variants {} ;
lin power_set_Q205170_QN = mkQN "potensmängd" ;
lin 'pre-measure_Q1393014_QN' = variants {} ;
lin prime_element_Q240651_QN = mkQN "Primelement" ;
lin prime_ideal_Q863912_QN = mkQN "Primideal" ;
lin prime_number_Q49008_QN = mkQN "primtal" ;
lin principal_ideal_Q44382_QN = mkQN "principalideal" ;
lin principal_ideal_domain_Q1143969_QN = mkQN "principalidealdomän" ;
lin product_measure_Q1572094_QN = mkQN "Produktmått" ;
lin projection_Q519967_QN = variants {} ;
lin projectivization_Q7249479_QN = variants {} ;
lin pullback_Q978505_QN = variants {} ;
lin pushout_Q1633079_QN = variants {} ;
lin quadratic_integer_Q803531_QN = variants {} ;
lin quaternion_Q173853_QN = mkQN "kvaternion" ;
lin quotient_set_Q3966112_QN = variants {} ;
lin quotient_group_Q1138961_QN = mkQN "kvotgrupp" ;
lin quotient_algebra_Q2589508_QN = variants {} ;
lin quotient_module_Q1432554_QN = variants {} ;
lin quotient_topological_space_Q1139111_QN = variants {} ;
lin quotient_ring_Q619436_QN = mkQN "Kvotring" ;
lin Radon_measure_Q2126650_QN = mkQN "Radonmått" ;
lin rank_Q656784_QN = mkQN "Matrisrang" ;
lin rank_of_an_abelian_group_Q1260803_QN = variants {} ;
lin real_projective_plane_Q633815_QN = variants {} ;
lin symplectic_group_Q936434_QN = variants {} ;
lin rectangle_Q209_QN = mkQN "rektangel" ;
lin rectifiable_set_Q3038340_QN = variants {} ;
lin reduced_homology_Q7306331_QN = variants {} ;
lin refinement_Q55631370_QN = variants {} ;
lin regular_measure_Q1428110_QN = variants {} ;
lin Relative_cycle_Q55643339_QN = variants {} ;
lin relative_homology_Q1959911_QN = variants {} ;
lin Riemann_integral_Q697181_QN = mkQN "Riemannintegral" ;
lin Riemann_sphere_Q825857_QN = mkQN "Riemannsfären" ;
lin coset_Q751969_QN = mkQN "Sidoklass" ;
lin ring_Q161172_QN = mkQN "ring" ;
lin ring_homomorphism_Q1194212_QN = variants {} ;
lin normed_ring_Q110571247_QN = variants {} ;
lin ring_of_sets_Q2064647_QN = variants {} ;
lin rng_Q17102802_QN = variants {} ;
lin rule_of_product_Q557624_QN = mkQN "Multiplikationsprincipen" ;
lin rule_of_sum_Q2360148_QN = variants {} ;
lin Schwartz_space_Q1369621_QN = variants {} ;
lin 'second-countable_space_Q1363919_QN' = variants {} ;
lin 'self-adjoint_operator_Q6500908_QN' = variants {} ;
lin 'semi-locally_simply_connected_space_Q7449310_QN' = variants {} ;
lin semidirect_product_Q291126_QN = variants {} ;
lin semigroup_Q207348_QN = mkQN "semigrupp" ;
lin semisimple_Lie_algebra_Q2366896_QN = variants {} ;
lin Semisimple_representation_Q81376506_QN = variants {} ;
lin convergent_sequence_Q2996364_QN = variants {} ;
lin sequentially_compact_space_Q1135427_QN = variants {} ;
lin parity_of_a_permutation_Q1064405_QN = mkQN "Paritet" ;
lin simple_Lie_algebra_Q78502771_QN = variants {} ;
lin simplicial_complex_Q994399_QN = variants {} ;
lin simplicial_homology_Q7520902_QN = variants {} ;
lin skeleton_Q2291918_QN = variants {} ;
lin smooth_manifold_Q78338964_QN = variants {} ;
lin solvable_Lie_algebra_Q7558992_QN = variants {} ;
lin special_linear_Lie_algebra_Q7574831_QN = variants {} ;
lin special_linear_group_Q2140900_QN = variants {} ;
lin special_orthogonal_group_Q3117934_QN = variants {} ;
lin special_unitary_group_Q684363_QN = variants {} ;
lin Stirling_numbers_of_the_second_kind_Q2601117_QN = variants {} ;
lin structure_constants_Q7625098_QN = variants {} ;
lin subalgebra_Q629933_QN = variants {} ;
lin subgraph_Q7631151_QN = mkQN "delgraf" ;
lin subgroup_Q466109_QN = mkQN "delgrupp" ;
lin submodule_Q2498094_QN = variants {} ;
lin subrepresentation_Q55648664_QN = variants {} ;
lin subspace_topology_Q660730_QN = mkQN "Delrumstopologi" ;
lin support_Q1136376_QN = variants {} ;
lin surface_Q484298_QN = mkQN "yta" ;
lin Sylow_subgroup_Q78269866_QN = variants {} ;
lin symmetric_derivative_Q3773126_QN = variants {} ;
lin symmetric_power_Q55643402_QN = variants {} ;
lin tangent_space_Q909601_QN = mkQN "tangentrum" ;
lin tensor_product_of_representations_Q48995828_QN = variants {} ;
lin tensor_product_Q1163016_QN = mkQN "Tensorprodukt" ;
lin topological_field_Q10260753_QN = variants {} ;
lin topological_game_Q7825035_QN = variants {} ;
lin topological_ring_Q922339_QN = variants {} ;
lin 'torsion-free_module_Q7827200_QN' = variants {} ;
lin torsion_subgroup_Q2293759_QN = variants {} ;
lin total_variation_Q1936288_QN = variants {} ;
lin totally_bounded_space_Q1362228_QN = variants {} ;
lin totally_disconnected_space_Q1544261_QN = variants {} ;
lin trace_Q321102_QN = mkQN "Spår" ;
lin transposition_Q2666112_QN = mkQN "Transposition" ;
lin tree_Q272735_QN = mkQN "träd" ;
lin trivial_representation_Q7844662_QN = variants {} ;
lin unique_factorization_domain_Q1052579_QN = mkQN "EF-ring" ;
lin unit_Q118084_QN = variants {} ;
lin subring_Q929536_QN = variants {} ;
lin unitary_group_Q1500617_QN = variants {} ;
lin unitary_transformation_Q2495534_QN = variants {} ;
lin universal_cover_Q4475749_QN = variants {} ;
lin linear_subspace_Q728435_QN = mkQN "delrum" ;
lin Vitali_covering_lemma_Q3229352_QN = variants {} ;
lin volume_Q39297_QN = mkQN "volym" ;
lin weak_convergence_Q11861938_QN = variants {} ;
lin wedge_sum_Q1781358_QN = variants {} ;
lin Weight_space_Q72535212_QN = variants {} ;
lin Weyl_group_Q768074_QN = variants {} ;
lin width_Q35059_QN = mkQN "bredd" ;
lin zero_divisor_Q828111_QN = mkQN "Nolldelare" ;
lin 'sigma-algebra_Q217357_QN' = mkQN "Sigma-algebra" ;
lin 'Zero–one_law_Q14481419_QN' = variants {} ;
lin abelian_category_Q318737_QN = variants {} ;
lin abscissa_Q515874_QN = mkQN "abskissa" ;
lin absolute_convergence_Q332465_QN = mkQN "Absolutkonvergens" ;
lin absolutely_convergent_series_Q91134251_QN = variants {} ;
lin acnode_Q844451_QN = variants {} ;
lin addition_Q32043_QN = mkQN "addition" ;
lin additive_category_Q4681343_QN = variants {} ;
lin adjoint_functor_Q357858_QN = variants {} ;
lin affine_scheme_Q91435997_QN = variants {} ;
lin affine_hull_Q382504_QN = variants {} ;
lin affine_space_Q382698_QN = variants {} ;
lin algebraically_closed_field_Q1047547_QN = variants {} ;
lin algorithm_Q8366_QN = mkQN "algoritm" ;
lin analytic_set_Q485312_QN = variants {} ;
lin 'Arzelà–Ascoli_theorem_Q1477053_QN' = variants {} ;
lin autonomous_system_Q788009_QN = variants {} ;
lin barycenter_Q809690_QN = mkQN "barycentrum" ;
lin Bases_Q4866725_QN = mkQN "Bases" ;
lin bilinear_form_Q837924_QN = mkQN "bilinjär" ;
lin bisection_method_Q866300_QN = mkQN "Bisektionsmetoden" ;
lin Boolean_algebra_Q4973304_QN = variants {} ;
lin proof_by_exhaustion_Q9299950_QN = variants {} ;
lin cartesian_monoidal_category_Q18205845_QN = variants {} ;
lin chain_rule_Q17004731_QN = variants {} ;
lin change_of_basis_Q810255_QN = mkQN "Basbyte" ;
lin coordinate_system_Q11210_QN = mkQN "koordinatsystem" ;
lin change_of_variables_Q1934165_QN = mkQN "Variabelbyte" ;
lin characteristic_Q836088_QN = mkQN "Karakteristik" ;
lin clopen_set_Q320369_QN = variants {} ;
lin coherent_sheaf_Q906907_QN = variants {} ;
lin conditioning_Q5159291_QN = variants {} ;
lin Connected_component_Q230646_QN = variants {} ;
lin consistency_Q1319773_QN = mkQN "konsistens" ;
lin 'list_of_continuity-related_mathematical_topics_Q5165409_QN' = variants {} ;
lin continuous_function_Q5165476_QN = variants {} ;
lin contour_integration_Q856103_QN = mkQN "Residykalkyl" ;
lin convergence_of_random_variables_Q578985_QN = variants {} ;
lin convergent_series_Q1211057_QN = variants {} ;
lin convolution_Q210857_QN = mkQN "faltning" ;
lin counting_measure_Q247204_QN = mkQN "Kardinalitetmått" ;
lin cover_Q331481_QN = variants {} ;
lin vector_product_Q178192_QN = mkQN "kryssprodukt" ;
lin curve_Q161973_QN = mkQN "kurva" ;
lin arc_length_Q670036_QN = mkQN "båglängd" ;
lin cyclotomic_polynomial_Q1051983_QN = mkQN "cirkeldelningspolynom" ;
lin definable_set_Q2895824_QN = variants {} ;
lin derivative_Q29175_QN = mkQN "derivata" ;
lin Diagonalisierung_Q1208191_QN = mkQN "Diagonalisierung" ;
lin differentiable_function_Q783507_QN = mkQN "differentierbarhet" ;
lin Dirac_comb_Q385599_QN = variants {} ;
lin direct_product_of_groups_Q2725924_QN = variants {} ;
lin disjoint_union_Q5282259_QN = variants {} ;
lin Dirichlet's_theorem_Q1217467_QN = variants {} ;
lin distribution_Q865811_QN = mkQN "distribution" ;
lin distribution_Q564835_QN = variants {} ;
lin probability_distribution_Q200726_QN = mkQN "sannolikhetsfördelning" ;
lin dual_basis_Q2297777_QN = mkQN "Dualbas" ;
lin graph_distance_Q2742711_QN = variants {} ;
lin eccentricity_Q104486_QN = mkQN "excentricitet" ;
lin Event_Q1297532_QN = mkQN "Event" ;
lin exponential_object_Q4391173_QN = variants {} ;
lin extreme_point_Q1385465_QN = variants {} ;
lin Fields_Q1201101_QN = mkQN "Fields" ;
lin finite_group_Q1057968_QN = variants {} ;
lin finitely_generated_algebra_Q2835968_QN = variants {} ;
lin focus_Q3568_QN = variants {} ;
lin Fourier_series_Q179467_QN = mkQN "Fourierserie" ;
lin Fourier_transform_Q6520159_QN = mkQN "fouriertransform" ;
lin free_algebra_Q5500180_QN = variants {} ;
lin Fubini's_theorem_Q1149022_QN = variants {} ;
lin Galois_group_Q730384_QN = mkQN "Galoisgrupp" ;
lin 'Gauss–Seidel_method_Q1069090_QN' = variants {} ;
lin Gaussian_elimination_Q2658_QN = mkQN "Gausselimination" ;
lin gradient_descent_Q1199743_QN = variants {} ;
lin Gronwall's_inequality_Q510733_QN = variants {} ;
lin height_Q208826_QN = mkQN "höjd" ;
lin heterogeneous_algebra_Q1616174_QN = variants {} ;
lin Hilbert_projection_theorem_Q3527215_QN = variants {} ;
lin independent_events_Q625303_QN = mkQN "Oberoende" ;
lin Induced_topology_Q17130880_QN = variants {} ;
lin injective_module_Q2716519_QN = variants {} ;
lin inscribed_angle_Q915985_QN = mkQN "randvinkel" ;
lin integral_Q80091_QN = mkQN "integral" ;
lin integration_by_parts_Q273328_QN = mkQN "partialintegration" ;
lin Fourier_inversion_theorem_Q3984053_QN = variants {} ;
lin inverse_trigonometric_function_Q674533_QN = mkQN "arcusfunktion" ;
lin isolated_singularity_Q2297037_QN = variants {} ;
lin LU_decomposition_Q833089_QN = mkQN "LU-faktorisering" ;
lin 'square-integrable_function_Q1530791_QN' = variants {} ;
lin Lagrange_multiplier_Q598870_QN = mkQN "Lagrangemultiplikator" ;
lin Laurent_series_Q425432_QN = mkQN "Laurentserie" ;
lin weak_formulation_Q9993851_QN = variants {} ;
lin list_of_things_named_after_Gottfried_Leibniz_Q394747_QN = variants {} ;
lin Limit_Q246639_QN = mkQN "Limit" ;
lin line_integral_Q467699_QN = mkQN "Kurvintegral" ;
lin 'Hartman–Grobman_theorem_Q211981_QN' = variants {} ;
lin Lipschitz_function_Q652707_QN = mkQN "Lipschitzkontinuitet" ;
lin maxima_and_minima_Q845060_QN = mkQN "extremum" ;
lin logarithm_Q11197_QN = mkQN "logaritm" ;
lin MacLaurin_series_Q1882442_QN = mkQN "Maclaurinserie" ;
lin function_Q11348_QN = mkQN "funktion" ;
lin Maschke's_theorem_Q656198_QN = variants {} ;
lin matrix_exponential_Q1191722_QN = mkQN "Matrisexponentialfunktion" ;
lin matrix_representation_Q6787889_QN = variants {} ;
lin maximum_principle_Q1914255_QN = variants {} ;
lin mean_value_theorem_Q189136_QN = mkQN "medelvärdessatsen" ;
lin element_Q379825_QN = mkQN "element" ;
lin meter_Q630649_QN = mkQN "taktart" ;
lin minimal_polynomial_Q2242730_QN = variants {} ;
lin minimal_polynomial_Q1163608_QN = mkQN "Minimalpolynom" ;
lin model_category_Q1941896_QN = variants {} ;
lin Moment_Q1451745_QN = mkQN "Moment" ;
lin morphism_Q1948412_QN = mkQN "morfism" ;
lin multilinear_map_Q1952404_QN = mkQN "multilinjär" ;
lin multiplicity_Q2228257_QN = mkQN "multiplicitet" ;
lin Newton's_identities_Q1749812_QN = variants {} ;
lin noetherian_scheme_Q3475675_QN = variants {} ;
lin 'non-empty_set_Q10533491_QN' = mkQN "icke-tom" ;
lin normal_scheme_Q7051829_QN = variants {} ;
lin number_Q11563_QN = mkQN "tal" ;
lin Orbit_Q230664_QN = variants {} ;
lin order_Q589491_QN = mkQN "Ordning" ;
lin Order_Q1820515_QN = variants {} ;
lin order_Q1431456_QN = variants {} ;
lin orientation_Q1052451_QN = variants {} ;
lin Parseval's_theorem_Q1443036_QN = variants {} ;
lin partial_fraction_decomposition_Q431617_QN = mkQN "partialbråksuppdelning" ;
lin partial_function_Q1756942_QN = variants {} ;
lin perfect_field_Q2997817_QN = variants {} ;
lin phase_portrait_Q2532943_QN = variants {} ;
lin pointwise_convergence_Q1778098_QN = variants {} ;
lin polar_decomposition_Q2101158_QN = mkQN "Polärfaktorisering" ;
lin measure_Q192276_QN = mkQN "mått" ;
lin power_series_Q206925_QN = mkQN "potensserie" ;
lin probability_density_function_Q207522_QN = mkQN "täthetsfunktion" ;
lin probability_measure_Q355020_QN = variants {} ;
lin product_Q919107_QN = variants {} ;
lin product_ring_Q3406712_QN = variants {} ;
lin projective_module_Q942423_QN = variants {} ;
lin quasicoherent_sheaf_Q61040808_QN = variants {} ;
lin radius_of_convergence_Q1428097_QN = mkQN "Konvergensradie" ;
lin rate_of_convergence_Q1783502_QN = variants {} ;
lin regular_polygon_Q714886_QN = variants {} ;
lin residue_theorem_Q830513_QN = mkQN "residysatsen" ;
lin Riemann_sum_Q1156903_QN = mkQN "Riemannsumma" ;
lin 'Riemann–Lebesgue_lemma_Q9363284_QN' = variants {} ;
lin scheme_Q1155772_QN = variants {} ;
lin Schur's_lemma_Q1816952_QN = variants {} ;
lin series_Q170198_QN = mkQN "serie" ;
lin set_Q36161_QN = mkQN "mängd" ;
lin sheaf_Q595298_QN = mkQN "kärve" ;
lin Signature_Q298582_QN = mkQN "Signature" ;
lin signature_Q7512811_QN = variants {} ;
lin singular_value_decomposition_Q420904_QN = mkQN "Singulärvärdesuppdelning" ;
lin stability_theory_Q1756677_QN = variants {} ;
lin stability_Q7595717_QN = mkQN "stabilitet" ;
lin stationary_set_Q1528401_QN = variants {} ;
lin sum_Q218005_QN = mkQN "summa" ;
lin symmetric_algebra_Q1052674_QN = variants {} ;
lin symmetric_polynomial_Q930499_QN = variants {} ;
lin Taylor_series_Q131187_QN = mkQN "Taylorserie" ;
lin tensor_algebra_Q2296021_QN = mkQN "tensoralgebra" ;
lin term_algebra_Q21998744_QN = variants {} ;
lin tessellation_Q214856_QN = mkQN "tessellation" ;
lin transitive_set_Q671944_QN = variants {} ;
lin triangulated_category_Q7840150_QN = variants {} ;
lin triple_product_Q36248_QN = mkQN "trippelprodukt" ;
lin uncountable_set_Q1128796_QN = mkQN "överuppräknelig" ;
lin variable_Q50701_QN = mkQN "variabel" ;
lin winding_number_Q576728_QN = mkQN "omloppstal" ;
}
