#!/bin/bash

# Start MCP server in background
export MONICA_API_URL=http://localhost:8081/api
export MONICA_API_TOKEN=eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJhdWQiOiIxIiwianRpIjoiMGNkNzE2MmYxNzQ0MmYxNGNmMTg1Y2E1OWQ4NWQzNjIyZGU2YTVlNGY5YTk2Y2EyYTgyNjIwNjQ3MDNjMDI5YWNmZjk2Y2VlMjE1MjFmNmEiLCJpYXQiOjE3NjAwMTgwMjMuOTgyNzc2LCJuYmYiOjE3NjAwMTgwMjMuOTgyNzc4LCJleHAiOjE3OTE1NTQwMjMuOTY2ODY5LCJzdWIiOiIzIiwic2NvcGVzIjpbXX0.dtgOLLXZHoRML0myIs-uGSjY2SpH3og4c3NPhzYAPNar146tjOdD36SFjMtMcXkgDEMyCQA7JjHaJY6S1wnhhC_TX4frjsrjQzXvN_pe00VdDKzfUcGXRuO_Kwjds3ZEHlU9ts37DhtiUKvxzq_NFKO_WYRd6dfIrPR-g__x4PLDjdq7NcimumF_9czw2Wu-sjisTjzwu4MPhlko1W-1HRvyv7EWRQ0XI__5fDgUeoBnP55kgdE9j7AUNQQDKUuPBRzhSWXTz5vTA94GsTPe3hI4deIP6_bL7lzAeA7GwBYODRxdvDD-BUKArhbqTo5N3g6ihpa4YwBZhH3_EDLgYMmHcAPQeGgEKLerAK2R9K5AD5OfrVtwh6_HFmF0jss38ZCDhsIjUS3Htl0JusScQ8-7wMRVKNULgpk44fbmOk8QIUtK-w0OXwe0R0caEPNOW1mKYbEbeFy7OYWm2b9TNjCRjXkzrBZRTCdMfTHFsBKB_D9U_C_l-F-Gs6cxh0J9dwVuYN7IaYeP42BEet6m927eXtaw7SHR6E9sBHdmEuQdXRMyjRgbg2Eoly0t7LIpzb_hMtMnl24vaqtlZp8ngDZtJk9fnABKg682ieHPX1Y0v0190C-eGKfEHtuDow2mkLLqjNnSAZuPRl4XkNRNgbJ9FAcH5hEFtPxifRkjb9M

java -jar build/libs/monicahq-mcp-1.0.0.jar 2>&1 | tee /tmp/mcp-server.log &
SERVER_PID=$!

sleep 3

# Send test request using stdio
echo '{"jsonrpc": "2.0", "id": 1, "method": "tools/call", "params": {"name": "contact_field_create", "arguments": {"contactId": 91, "contactFieldTypeId": 15, "data": "test-mcp@example.com"}}}' | java -jar build/libs/monicahq-mcp-1.0.0.jar 2>&1

# Cleanup
kill $SERVER_PID 2>/dev/null || true
