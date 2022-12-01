import JSONValue, { JSONType } from "../utils/json";
import Address, { Address as AddressType } from "./address";

export type TicketID = {
  ticketer: AddressType;
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

const toDTO = (ticket: TicketID) => {
  let data = ticket.data;
  if (data.startsWith("0x")) data = data.slice(2);

  return [
    "Ticket_id",
    {
      ticketer: ticket.ticketer,
      data: data,
    },
  ];
};

const parseTicketID = (str: String): TicketID => {
  const split = str.split(" ").filter((x) => x);
  if (split.length != 2) {
    throw Error(`Incorrect argument for TicketID: ${str}`);
  }

  // TODO test that data is valid
  return { ticketer: Address.parseAddress(split[0]), data: split[1] };
};

export default {
  createTicketID,
  ofDTO,
  toDTO,
  parseTicketID,
};
