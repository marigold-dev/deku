import JSONValue, { JSONType } from "../utils/json";
import TicketID, { TicketID as TicketIDType } from "./ticket-id";

export type TicketAmount = {
  ticket: TicketIDType;
  amount: number;
};

export type Balances = TicketAmount[];

const ofDTO = (json: JSONValue): Balances | null => {
  const jsonBalances = json.at("ledger").as_array();

  if (!jsonBalances) return null;
  const balances = jsonBalances.flatMap((entry: JSONValue) => {
    const ticket = TicketID.ofDTO(entry.at("ticket_id"));
    const amount = entry.at("amount").as_int();

    if (ticket === null || amount === null) throw Error("Incorrect Ticket_id");
    return [{ ticket, amount } as TicketAmount];
  });

  return balances;
};

export default {
  ofDTO,
};
