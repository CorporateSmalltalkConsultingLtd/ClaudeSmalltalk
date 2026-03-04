#!/usr/bin/env python3
"""Seagull Smalltalk MQTT Bridge — talk to a live Seagull image via MQTT.

Uses the SeagullLLMHandler running inside Seagull Smalltalk, communicating
over MQTT through the M0659 Paho client interface.

Seagull's JsonObjectEncoder/Decoder uses a custom format:
  {"Dictionary":{"contents":[["key","value"],...]}}
This script translates between standard Python dicts and that format.
"""

import argparse
import json
import sys
import threading
import time
import uuid

import paho.mqtt.client as mqtt

# Defaults — override via env or args
DEFAULT_BROKER = "localhost"
DEFAULT_PORT = 1883
DEFAULT_USER = ""
DEFAULT_PASS = ""
DEFAULT_IMAGE_ID = "seagull1"
DEFAULT_TIMEOUT = 15


def to_seagull(obj):
    """Convert a Python dict to Seagull's JSON Dictionary format."""
    if isinstance(obj, dict):
        contents = []
        for k, v in obj.items():
            contents.append([k, to_seagull(v)])
        return {"Dictionary": {"contents": contents}}
    if isinstance(obj, list):
        return [to_seagull(x) for x in obj]
    return obj


def from_seagull(obj):
    """Convert Seagull's JSON Dictionary format back to Python dict."""
    if isinstance(obj, dict):
        if "Dictionary" in obj:
            result = {}
            for pair in obj["Dictionary"]["contents"]:
                result[pair[0]] = from_seagull(pair[1])
            return result
        if "ByteArray" in obj:
            return bytes(obj["ByteArray"]["contents"]).decode("utf-8", "replace")
        return {k: from_seagull(v) for k, v in obj.items()}
    if isinstance(obj, list):
        return [from_seagull(x) for x in obj]
    return obj


class SeagullBridge:
    def __init__(self, broker=DEFAULT_BROKER, port=DEFAULT_PORT,
                 user=DEFAULT_USER, password=DEFAULT_PASS,
                 image_id=DEFAULT_IMAGE_ID, timeout=DEFAULT_TIMEOUT):
        self.broker = broker
        self.port = port
        self.image_id = image_id
        self.timeout = timeout
        self._result = {}
        self._event = threading.Event()

        self.client = mqtt.Client()
        self.client.username_pw_set(user, password)
        self.client.on_message = self._on_message
        self.client.connect(broker, port, 60)
        self.client.loop_start()

    def _on_message(self, client, userdata, msg):
        try:
            self._result = json.loads(msg.payload.decode("utf-8"))
        except Exception:
            self._result = {"raw": msg.payload.decode("utf-8", "replace")[:4000]}
        self._event.set()

    def request(self, action, payload):
        """Send a request to SeagullLLMHandler and return the parsed response."""
        self._event.clear()
        self._result = {}
        rid = str(uuid.uuid4())[:8]

        response_topic = f"claude/response/{rid}"
        self.client.subscribe(response_topic, qos=1)
        time.sleep(0.2)

        req = to_seagull({
            "requestId": rid,
            "action": action,
            "payload": payload,
        })
        self.client.publish(
            f"claude/request/{self.image_id}",
            json.dumps(req),
            qos=1,
        )

        if self._event.wait(self.timeout):
            self.client.unsubscribe(response_topic)
            parsed = from_seagull(self._result)
            if "error" in parsed:
                return {"error": parsed["error"], "stack": parsed.get("stack", "")}
            return parsed
        return {"error": "timeout waiting for Seagull response"}

    def close(self):
        self.client.loop_stop()
        self.client.disconnect()


def main():
    parser = argparse.ArgumentParser(description="Seagull Smalltalk MQTT Bridge")
    parser.add_argument("--broker", default=DEFAULT_BROKER, help="MQTT broker host")
    parser.add_argument("--port", type=int, default=DEFAULT_PORT, help="MQTT broker port")
    parser.add_argument("--user", default=DEFAULT_USER)
    parser.add_argument("--password", default=DEFAULT_PASS)
    parser.add_argument("--image-id", default=DEFAULT_IMAGE_ID)
    parser.add_argument("--timeout", type=int, default=DEFAULT_TIMEOUT)
    parser.add_argument("--json", action="store_true", help="Output raw JSON")

    sub = parser.add_subparsers(dest="command")

    p = sub.add_parser("evaluate", aliases=["eval"], help="Evaluate Smalltalk code")
    p.add_argument("code", help="Smalltalk expression to evaluate")

    p = sub.add_parser("browse", help="Browse a class")
    p.add_argument("className", help="Class name")

    p = sub.add_parser("method-source", help="Get method source")
    p.add_argument("className", help="Class name")
    p.add_argument("selector", help="Method selector")

    p = sub.add_parser("define-class", help="Define or modify a class")
    p.add_argument("definition", help="Full class definition expression")

    p = sub.add_parser("define-method", help="Define or modify a method")
    p.add_argument("className", help="Class name")
    p.add_argument("source", help="Method source code")

    p = sub.add_parser("delete-method", help="Delete a method")
    p.add_argument("className", help="Class name")
    p.add_argument("selector", help="Method selector")

    p = sub.add_parser("delete-class", help="Delete a class")
    p.add_argument("className", help="Class name")

    p = sub.add_parser("list-classes", help="List classes")
    p.add_argument("prefix", nargs="?", default="", help="Optional prefix filter")

    p = sub.add_parser("hierarchy", help="Get superclass chain")
    p.add_argument("className", help="Class name")

    p = sub.add_parser("subclasses", help="Get direct subclasses")
    p.add_argument("className", help="Class name")

    p = sub.add_parser("save-image", help="Save the Smalltalk image")

    p = sub.add_parser("check", help="Check connectivity to Seagull")

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        sys.exit(1)

    bridge = SeagullBridge(
        broker=args.broker, port=args.port,
        user=args.user, password=args.password,
        image_id=args.image_id, timeout=args.timeout,
    )

    try:
        if args.command == "check":
            r = bridge.request("evaluate", {"code": "Date today printString"})
            if "error" in r:
                print(f"FAIL: {r['error']}")
                sys.exit(1)
            print(f"OK — Seagull responded: {r.get('result', r)}")
            sys.exit(0)

        cmd_map = {
            "evaluate": lambda: bridge.request("evaluate", {"code": args.code}),
            "eval": lambda: bridge.request("evaluate", {"code": args.code}),
            "browse": lambda: bridge.request("browse", {"className": args.className}),
            "method-source": lambda: bridge.request("methodSource", {"className": args.className, "selector": args.selector}),
            "define-class": lambda: bridge.request("defineClass", {"definition": args.definition}),
            "define-method": lambda: bridge.request("defineMethod", {"className": args.className, "source": args.source}),
            "delete-method": lambda: bridge.request("deleteMethod", {"className": args.className, "selector": args.selector}),
            "delete-class": lambda: bridge.request("deleteClass", {"className": args.className}),
            "list-classes": lambda: bridge.request("listClasses", {"prefix": args.prefix}),
            "hierarchy": lambda: bridge.request("hierarchy", {"className": args.className}),
            "subclasses": lambda: bridge.request("subclasses", {"className": args.className}),
            "save-image": lambda: bridge.request("saveImage", {}),
        }

        r = cmd_map[args.command]()

        if args.json:
            print(json.dumps(r, indent=2))
        elif "error" in r:
            print(f"Error: {r['error']}", file=sys.stderr)
            if r.get("stack"):
                print(f"\nStack:\n{r['stack']}", file=sys.stderr)
            sys.exit(1)
        else:
            result = r.get("result", r)
            if isinstance(result, dict):
                # browse response
                if "name" in result:
                    print(f"Class: {result['name']}")
                    print(f"Superclass: {result.get('superclass', '?')}")
                    print(f"Instance variables: {', '.join(result.get('instanceVariables', []))}")
                    print(f"Class variables: {', '.join(result.get('classVariables', []))}")
                    print(f"Methods ({len(result.get('methods', []))}):")
                    for m in result.get("methods", []):
                        print(f"  {m}")
                else:
                    print(json.dumps(result, indent=2))
            elif isinstance(result, list):
                for item in result:
                    print(item)
            else:
                print(result)
    finally:
        bridge.close()


if __name__ == "__main__":
    main()
