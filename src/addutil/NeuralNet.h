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

#ifndef ADDUTILNEURALNET_H
#define ADDUTILNEURALNET_H

#include <list>
#include <vector>
#include <iostream>
#include <exception>

#include <boost/shared_ptr.hpp>

#include "General.h"

namespace addutil
{
    typedef float Weight;
    typedef float Value;
    typedef std::list<Weight> Neuron;
    typedef std::list<Neuron> NeuronLayer;
    typedef std::list<NeuronLayer> LayerList;
    typedef std::vector<int> NetTopology;
    typedef std::vector<Value> ValueList;
    typedef boost::shared_ptr<ValueList> ValueListPtr;
    typedef std::vector<Weight> WeightList;

    class NeuralNet
    {
    public:
        NeuralNet(const NetTopology& layers, float min_w = -1.0f, float max_w = 1.0f);
        ValueListPtr run(const ValueListPtr input) const;
        WeightList getWeights() const;
        void setWeights(const WeightList& ws);

    private:
        ValueListPtr layer_output(const NeuronLayer& l, const ValueListPtr v) const;
        Value neuron_output(const Neuron& n, const ValueListPtr v) const;

        LayerList m_layers;

    };
}

#endif
