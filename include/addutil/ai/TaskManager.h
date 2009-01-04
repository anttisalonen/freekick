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


#ifndef ADDUTIL_AI_TASKMANAGER_H
#define ADDUTIL_AI_TASKMANAGER_H

#include <vector>
#include <map>

#include <boost/shared_ptr.hpp>

#include "ai/Task.h"

namespace addutil
{
    namespace ai
    {
        typedef std::map<int, boost::shared_ptr<Task> > TaskList;
        class TaskManager
        {
        public:
            virtual ~TaskManager() { }
            int addTask(const boost::shared_ptr<Task>& t);
            bool deleteTask(int id);
        protected:
            TaskManager();
        private:
            TaskList tasks;
            int nextid;
        };
    }
}

#endif
