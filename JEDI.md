
# C++ code snippets from JEDI (a total of 3)

(FYI, the JEDI repositories can be found at https://github.com/JCSDA. The code challenge will only cover the following 3 code snippets)

## 1. fv3jediVar.cc
https://github.com/JCSDA/fv3-jedi/blob/master/src/mains/fv3jediVar.cc
```
/*                                                                                                                                                                     
 * (C) Copyright 2017-2020 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "fv3jedi/Utilities/Traits.h"
#include "oops/generic/instantiateModelFactory.h"
#include "oops/runs/Run.h"
#include "saber/oops/instantiateCovarFactory.h"
#include "ufo/instantiateObsErrorFactory.h"
#include "ufo/instantiateObsFilterFactory.h"
#include "ufo/ObsTraits.h"

#include "oops/runs/Variational.h"

int main(int argc,  char ** argv) {
  oops::Run run(argc, argv);
  saber::instantiateCovarFactory<fv3jedi::Traits>();
  ufo::instantiateObsErrorFactory();
  ufo::instantiateObsFilterFactory();
  oops::instantiateModelFactory<fv3jedi::Traits>();
  oops::Variational<fv3jedi::Traits, ufo::ObsTraits> var;
  return run.execute(var);
}

```

## 2. Classes
```
class CovarianceMatrix {
 public:
  CovarianceMatrix(const Geometry &, const Configuration &);
  ~CovarianceMatrix();
  virtual Increment multiply(const Increment &) const =0;
}

class SpectralCovariance : public CovarianceMatrix {
 public:
  CovarianceMatrix(const Geometry &, const Configuration &);
  ~CovarianceMatrix();
  Increment multiply(const Increment &) const override;
}

class WaveletCovariance : public CovarianceMatrix {
 public:
  CovarianceMatrix(const Geometry &, const Configuration &);
  ~CovarianceMatrix();
  Increment multiply(const Increment &) const override;
}

```

## 3. Factories
```
// Vector of pointers to hold the filters
std::vector< std::shared_ptr< ObsFilterBase<MODEL> > > filters_;

// Get filters configuration
std::vector<eckit::LocalConfiguration> confs;
conf.get("ObsFilters", confs);

// Create the filters
for (std::size_t jj = 0; jj < confs.size(); ++jj) {
  std::shared_ptr< ObsFilterBase<MODEL> > filtr(
    FilterFactory<MODEL>::create(os, confs[jj], qcflags, obserr)
  );
  filters_.push_back(filtr);
}

```
