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

#include "TaskManager.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                TaskManager::TaskManager ()
                    : nextid(1)
                {
                }

                int TaskManager::addTask(const boost::shared_ptr<tasks::Task>& t)
                {
                    mTasks[nextid] = t;
                    nextid++;
                    return (nextid - 1);
                }

                bool TaskManager::deleteTask(int id)
                {
                    return (mTasks.erase(id) == 1);
                }

                bool TaskManager::deleteNextTask()
                {
                    if(mTasks.empty()) return false;
                    TaskList::iterator it = mTasks.begin();
                    mTasks.erase(it);
                    return true;
                }

                boost::shared_ptr<messages::PlayerControlMessage> TaskManager::think()
                {
                    try
                    {
                        boost::shared_ptr<tasks::Task> thistask = getNextTask();
                        bool finished;
                        boost::shared_ptr<messages::PlayerControlMessage> msg = thistask->process(finished);
                        if(finished) deleteNextTask();
                        return msg;
                    }
                    catch(...)
                    {
                        // No tasks available
                        const char* errmsg = "ai_client::TaskManager::think: no tasks.\n";
                        std::cerr << errmsg;
                        throw errmsg;
                    }
                }

                boost::shared_ptr<tasks::Task> TaskManager::getNextTask()
                {
                    if(mTasks.empty()) throw "TaskManager::getNextTask: no mTasks";
                    TaskList::iterator it = mTasks.begin();
                    return it->second;
                }
            }
        }
    }
}
