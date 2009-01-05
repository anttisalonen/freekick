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


#ifndef FREEKICKCOMPOSITETASK_H
#define FREEKICKCOMPOSITETASK_H

#include <map>

#include <boost/shared_ptr.hpp>

#include "messages/PlayerControlMessage.h"

#include "tasks/Task.h"

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
                    typedef std::map<int, boost::shared_ptr<Task> > TaskList;
                    class CompositeTask : public Task
                    {
                    public:
                        virtual ~CompositeTask() { }
                        int addTask(const boost::shared_ptr<Task>& t);
                        bool deleteTask(int id);
                        bool deleteNextTask();
                        boost::shared_ptr<Task> getNextTask() const;
                        boost::shared_ptr<Task> getTask(int id) const;
                        boost::shared_ptr<messages::PlayerControlMessage> process();
                    protected:
                        CompositeTask();
                    private:
                        int nextid;
                        TaskList mTasks;
                    };
                }
            }
        }
    }
}

#endif
