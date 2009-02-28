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


#ifndef ADDUTILGENETICALGORITHM_H
#define ADDUTILGENETICALGORITHM_H

#include <vector>
#include <list>
#include <ostream>
#include <iostream>

#include <boost/tuple/tuple.hpp>

#include "General.h"

namespace addutil
{
    namespace genetic_algorithm
    {
        typedef float Gene;
        typedef std::vector<Gene> Chromosome;
        typedef std::vector<Chromosome> Population;

        Gene newGene(Gene min = -1.0f, Gene max = 1.0f);
        Chromosome newChromosome(int num_genes, Gene min = -1.0f, Gene max = 1.0f);
        boost::tuple<Chromosome, Chromosome> crossover(const Chromosome& c1, const Chromosome& c2);
        template <typename T>
            int pickRoulette(const std::list<T>& p)
        {
            int tot = 0;
            int cumulative = 0;
            int rand_point = 0;
            int index = 0;
            int size = p.size();
            if(size == 0)
                throw "GA::pickRoulette(): received empty list";
            typename std::list<T>::const_iterator it;
            for(it = p.begin(); it != p.end(); it++)
            {
                tot += *it;
            }
            if(tot == 0)
            {
                return general::rand_int(size);
            }
            rand_point = general::rand_int(tot);
            for(index = 0, it = p.begin(); it != p.end(); it++, index++)
            {
                cumulative += *it;
                if (cumulative > rand_point)
                {
                    break;
                }
            }
            // std::cout << "roulette picked: " << index << std::endl;
            return index;
        }
        Chromosome mutate(const Chromosome& c, float prob, Gene min_change, Gene max_change, Gene min_val = -1.0f, Gene max_val = 1.0f);
        std::ostream& operator<<(std::ostream& os, const Chromosome& c);
    }
}

#endif
