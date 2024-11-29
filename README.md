# Geomagnetic Depth Sounding 3D Modeling and Inversion Toolkit

## Overview

This toolkit comprises a suite of Fortran 90 (F90) codes designed for 3D Modeling and Inversion in Geomagnetic Depth Sounding. It enables users to conduct comprehensive three-dimensional forward and inverse geomagnetic depth sounding calculations with high efficiency and accuracy.

## Getting Started

### Prerequisites

To utilize this toolkit, users must first install the Intel® oneAPI Base Toolkit and the Intel® HPC Toolkit to ensure compatibility and performance. Additionally, specific libraries are required:

- **TetGen**: Necessary for mesh generation. Available for download and compilation at [TetGen Official Website](https://wias-berlin.de/software/tetgen/).
- **MUMPS**: A crucial solver library. Download and compile from [MUMPS Official Website](https://mumps-solver.org/index.php?page=home).
- **Bisect**: This library is included within the `lib` directory of the toolkit.

### Structure of the Toolkit

The toolkit is organized into three primary directories: `lib`, `src`, and `vs`, each serving a distinct purpose:

- `lib`: This directory houses essential libraries that the toolkit relies on, including the MUMPS solver, bisect, and TetGen libraries, among others.
- `src`: Here, you'll find the source code, organized into subdirectories by functionality—Common, Data, Forward, Inverse, Main, Model, Primary, Solver, etc.
- `vs`: This directory contains the Intel Fortran project file along with example files to help users get started.

#### Key Example Files

- Mesh Files (`*.ele`, `*.node`): Define the tetrahedral elements and nodes of the model in TetGen format.
- Model Attributes (`*.attribute`): Detail the initial electrical properties of the model's regions.
- Control Parameters (`*.ctrl`): Specify algorithm control parameters.
- Data Files (`*.data`): Outline data formats for forward calculations and provide observational data for inversion.
- Source Information (`*.source`): Describe the electromagnetic field source.
- Task Specification (`task.dat`): Predefine task information.
- Surface Conductance Data (`newsedmap.txt`): Offer surface conductance data essential for accurate modeling. This data is derived from the study by Everett, M.E., S. Constable, and C.G. Constable, 2003, titled "Effects of near-surface conductance on global satellite induction responses," published in Geophysical Journal International, volume 153, pages 277–286.

#### Output Files

- Forward Calculation Results (`*.o.data`): Output from forward calculations.
- Inversion Results (`*.res`, `BFGS_rms_record.txt`, `*.vtk`): Key files detailing inversion outcomes and parameter records.

## Installation

Follow the installation instructions for the Intel® oneAPI Base Toolkit and Intel® HPC Toolkit closely. For TetGen and MUMPS libraries, refer to their respective official websites for download and compilation instructions. The Bisect library is already included in the `lib` directory.

## How to Use

Refer to the example files and `.ctrl` documents in the `vs` folder for detailed instructions on setting up and running your models.

## Contributing

We encourage contributions! Please submit Pull Requests or report issues via email to maxp20@mails.jlu.edu.cn.

## About the Author

- **Name**: Xinpeng Ma
- **Institution**: Jilin University
- **Contact**: maxp20@mails.jlu.edu.cn

## License

This toolkit is released under the GPL License. For full license details, please consult the LICENSE file.

---

### Additional Information

This toolkit represents a significant advancement in the field of geomagnetic research, offering researchers and practitioners a powerful tool for exploring the Earth's subsurface electrical properties. Its development and ongoing improvement are a testament to the collaborative effort of the geomagnetic research community. We welcome feedback, suggestions, and contributions to further enhance its capabilities and usability.
