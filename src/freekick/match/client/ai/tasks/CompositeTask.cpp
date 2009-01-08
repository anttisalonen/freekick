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

#include "tasks/CompositeTask.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                namespace tasks
                {
                    CompositeTask::CompositeTask ()
                        : nextid(1)
                    {
                    }

                    int CompositeTask::addTask(const boost::shared_ptr<Task>& t)
                    {
                        mTasks[nextid] = t;
                        nextid++;
                        // std::cout << "CompositeTask::addTask: size of mTasks: " << mTasks.size() << std::endl;
                        return (nextid - 1);
                    }

                    bool CompositeTask::deleteTask(int id)
                    {
                        // std::cout << "CompositeTask::deleteTask: size of mTasks (before deletion): " << mTasks.size() << std::endl;
                        return (mTasks.erase(id) == 1);
                    }

                    bool CompositeTask::deleteNextTask()
                    {
                        if(mTasks.empty()) return false;
                        // std::cout << "CompositeTask::deleteTask: size of mTasks (before deletion): " << mTasks.size() << std::endl;
                        TaskList::iterator it = mTasks.begin();
                        mTasks.erase(it);
                        return true;
                    }

                    void CompositeTask::clearTasks()
                    {
                        mTasks.clear();
                    }

                    boost::shared_ptr<Task> CompositeTask::getNextTask() const
                    {
                        if(mTasks.empty()) throw "CompositeTask::getNextTask: no mTasks\n";
                        TaskList::const_iterator it = mTasks.begin();
                        return it->second;
                    }

                    boost::shared_ptr<Task> CompositeTask::getTask(int id) const
                    {
                        TaskList::const_iterator it = mTasks.find(id);
                        if(it == mTasks.end()) throw "CompositeTask::getTask: Task not found\n";
                        return it->second;
                    }

                    bool CompositeTask::emptyTasks() const
                    {
                        return (mTasks.empty());
                    }

                    bool CompositeTask::finished() const
                    {
                        if(mTasks.empty()) return true;

                        TaskList::const_iterator it;
                        for(it = mTasks.begin(); it != mTasks.end(); it++)
                        {
                            if(!it->second->finished()) return false;
                        }
                        if(it == mTasks.end()) return true;

                        return false;
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> CompositeTask::process()
                    {
                        boost::shared_ptr<Task> nexttask = getNextTask();
                        if(nexttask->finished())
                        {
                            deleteNextTask();
                            return process();
                        }
                        boost::shared_ptr<messages::PlayerControlMessage> msg = nexttask->process();
                        return msg;
                    }
                }
            }
        }
    }
}
