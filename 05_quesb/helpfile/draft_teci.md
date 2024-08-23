# Total Edge Contrast Index

The edge contrast index is a measure used in landscape ecology to quantify the degree of contrast between adjacent patches in a landscape. This is particularly important for studying the movement of wide-ranging species across ecosystem types.

## Focal Areas in Tropical TECI Analysis

The determination of a focal area in Total Edge Contrast Index (TECI) analysis depends critically on the taxa or ecological communities under consideration.

### Examples of Focal Areas in the Tropics

1. **Tropical Rainforests**
2. **Tropical Savannas**
3. **Mangrove Forests**
4. **Tropical Dry Forests**
5. **Mountain Cloud Forests**

The choice of focal area will significantly influence how edge contrast is defined and measured.

## Total Edge Contrast Index (TECI)

The edge contrast index weights each segment of an edge by the degree of contrast between adjacent patches. The weights range from 0 to 1, where:

- 0 represents no contrast
- 1 represents maximum contrast

At the patch level, the total patch perimeter is reduced proportionate to the degree of contrast and reported as a percentage of the total perimeter. For example:

- A patch with a 10% edge contrast index has very little contrast with its neighborhood (e.g., primary mangrove forest next to secondary mangrove forest)
- A patch with a 90% edge contrast index has high contrast with its neighborhood (e.g., primary rainforest next to cleared agricultural land)

The Total Edge Contrast Index (TECI) quantifies edge contrast as a percentage of the maximum possible for the tropical landscape as a whole, considering the specific focal area of interest.

### Important Notes

1. This index is a relative measure.
2. Given any amount of edge, it measures the degree of contrast in that edge.
3. High values of ECON mean that the edge present, regardless of whether it is 10 m or 1,000 m, is of high contrast, and vice versa.
4. The interpretation of contrast can vary significantly depending on the focal ecosystem and taxa of interest.

## Establishing Edge Contrast Weights in Tropical Ecosystems

Edge contrast can have different meanings for various ecological processes in tropical ecosystems. When establishing contrast weights:

1. Weights must range between 0 (no contrast) and 1 (maximum contrast).
2. It is crucial to consider the specific requirements and sensitivities of the focal taxa or communities.
3. A reasoned guess based on theoretical understanding is better than assuming all edges are alike.

### Example Weighting Scheme for Different Tropical Ecosystems

Depending on the focal area, contrast weights might be based on:

1. Vegetation structure (e.g., canopy height in forests, grass height in savannas)
2. Species composition (e.g., pioneer vs. climax species in forests)
3. Moisture regime (e.g., terra firme vs. flooded forest, ephemeral vs. permanent water sources in savannas)
4. Degree of human disturbance (applicable across ecosystems)
5. Soil type and nutrient availability (particularly important in savanna-forest mosaics)

Process:
1. Establish a community (patch type) contrast key based on relevant factors for the focal ecosystem.
2. Assign each patch type a score (0-1) for each factor.
3. Compute contrast between patch types using weighted Euclidean distance.
4. Create a square symmetrical matrix showing contrast between each pair of patch types.

## Importance of Contrast in Tropical Ecosystems

Contrast between a patch and its neighborhood can influence several important ecological processes, with effects varying by ecosystem type:

1. **Transboundary Movements**: 
   - In forests: Hard edges may impede movement of canopy-dwelling species and seed dispersals
   - In savannas: Fire breaks or heavily grazed areas might affect herbivore movements

2. **Resource Utilization**: 
   - In forests: High-contrast edges may inhibit access to surrounding resources for interior species
   - In savannas: Water availability at ecosystem edges might concentrate herbivore activity

3. **Edge Effects**: 
   - In forests: Microclimatic changes affecting humidity-sensitive species
   - In savannas: Changes in fire regimes at ecosystem boundaries

4. **Species Preferences**: 
   - Some species prefer high contrast juxtapositions across different tropical ecosystems (e.g., savanna birds nesting in forest patches, forest raptors hunting in adjacent open areas)