/************************************************************************
  This file is part of Freekick.

  Freekick is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Freekick is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with Freekick.  If not, see <http://www.gnu.org/licenses/>.

  Copyright Antti Salonen, 2008
**************************************************************************/

#include "GeneticAlgorithm.h"

namespace addutil
{
    namespace genetic_algorithm
    {
        Gene newGene(Gene min, Gene max)
        {
            return general::rand_float(min, max);
        }

        Chromosome newChromosome(int num_genes, Gene min, Gene max)
        {
            Chromosome c;
            for(int i = 0; i < num_genes; i++)
                c.push_back(newGene(min, max));
            return c;
        }

        boost::tuple<Chromosome, Chromosome> crossover(const Chromosome& c1, const Chromosome& c2)
        {
            if(c1.size() != c2.size()) throw "GA::crossover: incompatible chromosomes";
            unsigned int p = general::rand_int(c1.size());
            Chromosome cr1, cr2;
            unsigned int i;
            for(i = 0; i < p; i++)
            {
                cr1.push_back(c1[i]);
                cr2.push_back(c2[i]);
            }
            for(i = p; i < c1.size(); i++)
            {
                cr1.push_back(c2[i]);
                cr2.push_back(c1[i]);
            }
            return boost::tuple<Chromosome, Chromosome>(cr1, cr2);
        }

        Chromosome mutate(const Chromosome& c, float prob, Gene min_change, Gene max_change, Gene min_val, Gene max_val)
        {
            Chromosome ret;
            Chromosome::const_iterator it;
            for(it = c.begin(); it != c.end(); it++)
            {
                if(general::rand_float() < prob)
                {
                    Gene orig = *it;
                    Gene newg = *it + general::rand_float(min_change, max_change);
                    if(newg < max_val && newg > min_val)
                        ret.push_back(newg);
                    else
                        ret.push_back(orig);
                }
                else
                {
                    ret.push_back(*it);
                }
            }
            return ret;
        }

        std::ostream& operator<<(std::ostream& os, const Chromosome& c)
        {
            Chromosome::const_iterator it;
            for(it = c.begin(); it != c.end(); it++)
            {
                os << *it << " ";
            }
            return os;
        }
    }
}
