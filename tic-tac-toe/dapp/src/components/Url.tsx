import { useRef, useState } from "react";

type UrlType = {
    address: string
}

const Url = ({ address }: UrlType) => {
    const [isCopied, setIsCopied] = useState(false);
    const timeout = useRef<any | null>(null);

    const url = `http://localhost:3000?game=${address}`;

    const copy = () => {
        navigator.clipboard.writeText(url);
        setIsCopied(true);
        if (!timeout.current) {
            timeout.current = setTimeout(() => {
                setIsCopied(false);
                timeout.current = null;
            }, 5000);
        }
    }

    return (
        <div id="url" onClick={copy}>
            {isCopied
                ? "Copied !"
                : url
            }
        </div>
    )
}

export default Url