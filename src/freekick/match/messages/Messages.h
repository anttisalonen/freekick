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


#ifndef FREEKICK_MESSAGES_H
#define FREEKICK_MESSAGES_H

#include "Message.h"
#include "ClientInitMessage.h"
#include "ConstantIntervalAck.h"
#include "ConstantUpdateMessage.h"
#include "GeneralError.h"
#include "GeneralIntervalAck.h"
#include "GeneralUpdateMessage.h"
#include "GeneralUpdatePauseMessage.h"
#include "GeneralUpdateRequest.h"
#include "GeneralUpdateScoreMessage.h"
#include "GeneralUpdateStatusMessage.h"
#include "GeneralUpdateTimeMessage.h"
#include "GetConstantUpdateInterval.h"
#include "GetGeneralUpdateInterval.h"
#include "GiveConstantUpdateIntervalMessage.h"
#include "GiveGeneralUpdateIntervalMessage.h"
#include "HeadPlayerControlMessage.h"
#include "HoldPlayerControlMessage.h"
#include "InitialDataMessage.h"
#include "InitialDataClubMessage.h"
#include "InitialDataKitMessage.h"
#include "InitialDataRequest.h"
#include "IntegerParameterMessage.h"
#include "KickPlayerControlMessage.h"
#include "ListOfPlayersMessage.h"
#include "ListParameterMessage.h"
#include "MovePlayerControlMessage.h"
#include "NewUsernameAck.h"
#include "ParameterMessage.h"
#include "PlayerControlMessage.h"
#include "PlayerControlRequestMessage.h"
#include "SerializationDataMessage.h"
#include "ServerInfoMessage.h"
#include "ServerInitMessage.h"
#include "SetConstantUpdateIntervalMessage.h"
#include "SetGeneralUpdateIntervalMessage.h"
#include "SingularMessage.h"
#include "StandardMessage.h"
#include "StatusPlayerControlMessage.h"
#include "StringParameterMessage.h"
#include "TacklePlayerControlMessage.h"
#include "UsernameRequestMessage.h"

#endif // FREEKICK_MESSAGES_H
