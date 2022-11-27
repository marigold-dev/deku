import JSONValue, { JSONType } from "../utils/json";

export type TicketID = {
  ticketer: string;
  data: string;
};

const createTicketID = (ticketer: string, data: string): TicketID => {
  return {
    ticketer,
    data,
  };
};

const ofDTO = (json: JSONValue): TicketID | null => {
  const ticket_id = json.as_array();

  if (!ticket_id) return null;

  const ticketer = ticket_id[1].at("ticketer").as_string();
  const data = ticket_id[1].at("data").as_string();

  if (ticketer === null || data === null) return null;

  return { ticketer, data };
};

const ofString = (str: String): TicketID | null => {
  const split = str.split(" ");
  if (split.length != 2) {
    throw Error(`Incorrect argument for TicketID: ${str}`);
  }

  // TODO test ticketer and data are valid
  return { ticketer: split[0], data: split[1] };
};

export default {
  createTicketID,
  ofDTO,
  ofString,
};
