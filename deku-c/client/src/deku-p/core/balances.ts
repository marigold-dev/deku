import Amount, { Amount as AmountType } from "./amount";
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

const toDTO = (balances: Balances) => {
  const DTO = balances.map((tamount) => {
    const ticket = TicketID.toDTO(tamount.ticket);
    const amount = tamount.amount.toString(); // FIXME please help
    return [ticket, amount];
  });
  return DTO;
};

const parseTicketAmount = (s: string): TicketAmount => {
  if (s[0] === "(") {
    if (!(s[s.length - 1] === ")")) throw Error(`Ill-formed pair: ${s}`);
    s = s.slice(1, -1);
  }
  const parts = s.split(" ").filter((x) => x);
  let i = 0;
  if (parts[0] == "Pair") i = 1;

  const ticketID = TicketID.parseTicketID(parts[i] + " " + parts[i + 1]);
  const amount = parseInt(parts[i + 2]);
  if (amount) return { ticket: ticketID, amount };
  else throw Error(`Incorrect amount: ${parts[i + 2]}`);
};

export default {
  ofDTO,
  toDTO,
  parseTicketAmount,
};
