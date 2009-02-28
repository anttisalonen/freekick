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

#include "NeuralNet.h"

namespace addutil
{
    NeuralNet::NeuralNet(const NetTopology& layers, float min_w, float max_w)
    {
        NetTopology::const_iterator it;
        if(layers.begin() == layers.end())
        {
            throw "NeuralNet::NeuralNet: too small net";
        }
        for(it = layers.begin() + 1; it != layers.end(); it++)
        {
            NeuronLayer nl;
            int num_neurons = *it;
            int num_neurons_prev_layer = *(it - 1);
            for(int i = 0; i < num_neurons; i++)
            {
                Neuron n;
                for(int j = 0; j < num_neurons_prev_layer; j++)
                {
                    n.push_back(addutil::general::rand_float(min_w, max_w));
                }
                nl.push_back(n);
            }
            m_layers.push_back(nl);
        }
    }

    ValueListPtr NeuralNet::run(const ValueListPtr input) const
    {
        LayerList::const_iterator it = m_layers.begin();
        ValueListPtr vl = layer_output(*it, input);
        it++;
        while(it != m_layers.end())
        {
            vl = layer_output(*it, vl);
            it++;
        }
        return vl;
    }

    ValueListPtr NeuralNet::layer_output(const NeuronLayer& l, const ValueListPtr v) const
    {
        NeuronLayer::const_iterator it;
        ValueListPtr retval(new ValueList);
        for(it = l.begin(); it != l.end(); it++)
        {
            retval->push_back(NeuralNet::neuron_output(*it, v));
        }
        return retval;
    }

    Value NeuralNet::neuron_output(const Neuron& n, const ValueListPtr v) const
    {
        Neuron::const_iterator it;
        ValueList::const_iterator vit;
        Value retval = 0.0f;

        for(it = n.begin(), vit = v->begin(); it != n.end() && vit != v->end(); it++, vit++)
        {
            retval += (*it) * (*vit);
        }
        // std::cout << retval << std::endl;
        return retval;
    }

    WeightList NeuralNet::getWeights() const
    {
        WeightList ret;
        LayerList::const_iterator it;
        for(it = m_layers.begin(); it != m_layers.end(); it++)
        {
            NeuronLayer::const_iterator it2;
            for(it2 = it->begin(); it2 != it->end(); it2++)
            {
                Neuron::const_iterator it3;
                for(it3 = it2->begin(); it3 != it2->end(); it3++)
                {
                    ret.push_back(*it3);
                }
            }
        }
        return ret;
    }

    void NeuralNet::setWeights(const WeightList& ws)
    {
        int index = 0;
        LayerList::iterator it;
        for(it = m_layers.begin(); it != m_layers.end(); it++)
        {
            NeuronLayer::iterator it2;
            for(it2 = it->begin(); it2 != it->end(); it2++)
            {
                Neuron::iterator it3;
                for(it3 = it2->begin(); it3 != it2->end(); it3++)
                {
                    *it3 = ws.at(index);
                    index++;
                }
            }
        }
    }
}
