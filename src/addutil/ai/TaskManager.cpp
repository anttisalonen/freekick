/************************************************************************
  This file is part of Freekick.

  Freekick is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Freekick is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Freekick.  If not, see <http://www.gnu.org/licenses/>.

  Copyright Antti Salonen, 2008
**************************************************************************/

#include "ai/TaskManager.h"

namespace addutil
{
    namespace ai
    {
        TaskManager::TaskManager ()
            : nextid(1)
        {
        }

        int TaskManager::addTask(const boost::shared_ptr<Task>& t)
        {
            tasks[nextid] = t;
            nextid++;
            return (nextid - 1);
        }

        bool TaskManager::deleteTask(int id)
        {
            return (tasks.erase(id) == 1);
        }
    }
}
