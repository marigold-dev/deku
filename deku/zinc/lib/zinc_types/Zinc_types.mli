module type S = Zinc_types_intf.S

module type Domain_types = Zinc_types_intf.Domain_types

module Make (D : Domain_types) :
  S
    with type Zinc.Hash.t := D.Hash.t
     and type Zinc.Address.t := D.Address.t
     and type Zinc.Contract.t := D.Contract.t
     and type Zinc.Chain_id.t := D.Chain_id.t
     and type Zinc.Ticket.t := D.Ticket.t
     and type Zinc.Key.t := D.Key.t
     and type Zinc.Key_hash.t := D.Key_hash.t

module Raw :
  S
    with type Zinc.Hash.t := string
     and type Zinc.Address.t := string
     and type Zinc.Contract.t := string * string option
     and type Zinc.Chain_id.t := string
     and type Zinc.Ticket.t := int64
     and type Zinc.Key.t := string
     and type Zinc.Key_hash.t := string
