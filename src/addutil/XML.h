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


#ifndef ADDUTIL_XML_H
#define ADDUTIL_XML_H

#include <string>

#include <libxml/parser.h>
#include <libxml/tree.h>

#include "General.h"

namespace addutil
{
    namespace xml
    {
        xmlNodePtr add_child(xmlNodePtr parent, const char* name)
        {
            return xmlNewChild(parent, NULL, BAD_CAST name, NULL);
        }

        void add_attribute(xmlNodePtr n, const char* name, const char* value)
        {
            xmlNewProp(n, BAD_CAST name, BAD_CAST value);
        }

        void add_attribute(xmlNodePtr n, const char* name, int value)
        {
            char f[8];
            sprintf(f, "%d", value);
            add_attribute(n, name, f);
        }

        void add_name_attribute(xmlNodePtr n, const char* value, const char* first_name_attr_name, const char* last_name_attr_name)
        {
            std::string first, last;
            general::name_to_first_and_last_name(value, first, last);
            add_attribute(n, first_name_attr_name, first.c_str());
            add_attribute(n, last_name_attr_name, last.c_str());
        }

        void color_to_xml(xmlNodePtr parent, char color, const char* node_name)
        {
            xmlNodePtr color_node = add_child(parent, node_name);
            int r, g, b;
            general::colorbyte_to_color(color, r, g, b);
            add_attribute(color_node, "r", r);
            add_attribute(color_node, "g", g);
            add_attribute(color_node, "b", b);
        }
    }
}

#endif
