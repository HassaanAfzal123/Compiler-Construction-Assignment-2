#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <iomanip>
using namespace std;

string join(const vector<string>& elements, const string& separator) {
    string result;
    for (size_t i = 0; i < elements.size(); i++) {
        result += elements[i];
        if (i < elements.size() - 1)
            result += separator;
    }
    return result;
}

vector<string> tokenize(const string &s) {
    vector<string> tokens;
    string token;
    bool inQuotes = false;
    for (size_t i = 0; i < s.size(); i++) {
        char c = s[i];
        if (c == '"') {
            if (inQuotes) {
                tokens.push_back(token);
                token.clear();
                inQuotes = false;
            } else {
                inQuotes = true;
            }
        } else if (isspace(c) && !inQuotes) {
            if (!token.empty()) {
                tokens.push_back(token);
                token.clear();
            }
        } else {
            token.push_back(c);
        }
    }
    if (!token.empty())
        tokens.push_back(token);
    return tokens;
}

map<string, vector<vector<string>>> readGrammar(const string &filename) {
    map<string, vector<vector<string>>> grammar;
    ifstream infile(filename);
    if (!infile) {
        cerr << "Error: Cannot open file " << filename << endl;
        exit(1);
    }
    string line;
    while(getline(infile, line)) {
        if(line.empty()) continue;
        size_t pos = line.find("->");
        if(pos == string::npos) continue;
        // trimming spaces
        string lhs = line.substr(0, pos);
        lhs.erase(0, lhs.find_first_not_of(" \t"));
        lhs.erase(lhs.find_last_not_of(" \t") + 1);
        
        string rhs = line.substr(pos + 2);
        istringstream prodStream(rhs);
        string prod;
        while(getline(prodStream, prod, '|')) {
            size_t start = prod.find_first_not_of(" \t");
            size_t end = prod.find_last_not_of(" \t");
            if(start == string::npos || end == string::npos)
                continue;
            string trimmed = prod.substr(start, end - start + 1);
            vector<string> tokens = tokenize(trimmed);
            grammar[lhs].push_back(tokens);
        }
    }
    return grammar;
}

void printGrammar(const map<string, vector<vector<string>>> &grammar) {
    for (auto &p : grammar) {
        cout << p.first << " -> ";
        bool firstProd = true;
        for (auto &prod : p.second) {
            if (!firstProd) cout << " | ";
            firstProd = false;
            for (auto &sym : prod) {
                if (sym == "ε" || sym == "╬╡")
                    cout << "\"ε\" ";
                else if (grammar.find(sym) == grammar.end())
                    cout << "\"" << sym << "\" ";
                else
                    cout << sym << " ";
            }
        }
        cout << endl;
    }
}

vector<string> commonPrefix(const vector<string> &p1, const vector<string> &p2) {
    vector<string> prefix;
    size_t len = min(p1.size(), p2.size());
    for (size_t i = 0; i < len; i++) {
        if (p1[i] == p2[i])
            prefix.push_back(p1[i]);
        else
            break;
    }
    return prefix;
}

map<string, vector<vector<string>>> leftFactorGrammar(const map<string, vector<vector<string>>> &grammar) {
    map<string, vector<vector<string>>> newGrammar = grammar;
    bool changes = true;
    int newNTCounter = 1;
    while(changes) {
        changes = false;
        map<string, vector<vector<string>>> tempGrammar;
        for (auto &ntPair : newGrammar) {
            string A = ntPair.first;
            vector<vector<string>> prods = ntPair.second;
            map<string, vector<vector<string>>> groups;
            for (auto &prod : prods) {
                if(prod.empty()) continue;
                string firstToken = prod[0];
                groups[firstToken].push_back(prod);
            }
            vector<vector<string>> newProds;
            for (auto &grp : groups) {
                if(grp.second.size() > 1) {
                    vector<string> prefix = grp.second[0];
                    for(auto &prod : grp.second) {
                        prefix = commonPrefix(prefix, prod);
                    }
                    if(prefix.size() >= 1) {
                        changes = true;
                        string newNT = A + "'" + to_string(newNTCounter++);
                        vector<string> newAProd = prefix;
                        newAProd.push_back(newNT);
                        newProds.push_back(newAProd);
                        vector<vector<string>> newNTProds;
                        for(auto &prod : grp.second) {
                            vector<string> remainder(prod.begin() + prefix.size(), prod.end());
                            if(remainder.empty())
                                remainder.push_back("ε");
                            newNTProds.push_back(remainder);
                        }
                        tempGrammar[newNT] = newNTProds;
                    }
                } else {
                    newProds.push_back(grp.second[0]);
                }
            }
            tempGrammar[A] = newProds;
        }
        newGrammar = tempGrammar;
    }
    return newGrammar;
}

map<string, vector<vector<string>>> removeIndirectLeftRecursion(const map<string, vector<vector<string>>> &grammar) {
    map<string, vector<vector<string>>> G = grammar;
    
    vector<string> nonTerms;
    for (auto &p : G)
        nonTerms.push_back(p.first);
    sort(nonTerms.begin(), nonTerms.end());
    
    int newNTCounter = 1;
    for (size_t i = 0; i < nonTerms.size(); i++) {
        string A = nonTerms[i];
        for (size_t j = 0; j < i; j++) {
            string B = nonTerms[j];
            vector<vector<string>> newProds;
            for (auto &prod : G[A]) {
                if (!prod.empty() && prod[0] == B) {
                    for (auto &delta : G[B]) {
                        vector<string> newProd = delta;
                        newProd.insert(newProd.end(), prod.begin() + 1, prod.end());
                        newProds.push_back(newProd);
                    }
                } else {
                    newProds.push_back(prod);
                }
            }
            G[A] = newProds;
        }
        vector<vector<string>> alpha;
        vector<vector<string>> beta;
        for (auto &prod : G[A]) {
            if (!prod.empty() && prod[0] == A) {
                vector<string> rem(prod.begin() + 1, prod.end());
                alpha.push_back(rem);
            } else {
                beta.push_back(prod);
            }
        }
        if (!alpha.empty()) {
            string newNT = A + "'" + to_string(newNTCounter++);
            vector<vector<string>> newAProds;
            for (auto &b : beta) {
                vector<string> newProd = b;
                newProd.push_back(newNT);
                newAProds.push_back(newProd);
            }
            G[A] = newAProds;
            vector<vector<string>> newNTProds;
            for (auto &a : alpha) {
                vector<string> newProd = a;
                newProd.push_back(newNT);
                newNTProds.push_back(newProd);
            }
            newNTProds.push_back(vector<string>{"ε"});
            G[newNT] = newNTProds;
            nonTerms.push_back(newNT);
            sort(nonTerms.begin(), nonTerms.end());
        }
    }
    return G;
}

map<string, set<string>> computeFirst(const map<string, vector<vector<string>>> &grammar) {
    map<string, set<string>> first;
    for (auto &p : grammar)
        first[p.first] = set<string>();
    bool changed = true;
    while (changed) {
        changed = false;
        for (auto &p : grammar) {
            string A = p.first;
            for (auto &prod : p.second) {
                bool deriveEpsilon = true;
                for (auto &symbol : prod) {
                    if (grammar.find(symbol) != grammar.end()) { // Non-terminal
                        size_t before = first[A].size();
                        for (auto &s : first[symbol]) {
                            if (s != "ε")
                                first[A].insert(s);
                        }
                        if (first[symbol].find("ε") == first[symbol].end()) {
                            deriveEpsilon = false;
                            break;
                        }
                        size_t after = first[A].size();
                        if (after > before)
                            changed = true;
                    } else { // Terminal (or epsilon)
                        size_t before = first[A].size();
                        if (symbol == "╬╡")
                            first[A].insert("ε");
                        else
                            first[A].insert(symbol);
                        size_t after = first[A].size();
                        if (after > before)
                            changed = true;
                        deriveEpsilon = false;
                        break;
                    }
                }
                if (deriveEpsilon) {
                    size_t before = first[A].size();
                    first[A].insert("ε");
                    size_t after = first[A].size();
                    if (after > before)
                        changed = true;
                }
            }
        }
    }
    return first;
}

map<string, set<string>> computeFollow(const map<string, vector<vector<string>>> &grammar, const map<string, set<string>> &first) {
    map<string, set<string>> follow;
    for (auto &p : grammar)
        follow[p.first] = set<string>();
    string startSymbol = (grammar.find("S") != grammar.end() ? "S" : grammar.begin()->first);
    follow[startSymbol].insert("$");
    bool changed = true;
    while (changed) {
        changed = false;
        for (auto &p : grammar) {
            string A = p.first;
            for (auto &prod : p.second) {
                for (size_t i = 0; i < prod.size(); i++) {
                    string B = prod[i];
                    if (grammar.find(B) != grammar.end()) {
                        size_t before = follow[B].size();
                        bool addFollowA = true;
                        for (size_t j = i + 1; j < prod.size(); j++) {
                            string beta = prod[j];
                            if (grammar.find(beta) != grammar.end()) {
                                for (auto s : first.at(beta)) {
                                    if (s != "ε")
                                        follow[B].insert(s);
                                }
                                if (first.at(beta).find("ε") == first.at(beta).end()) {
                                    addFollowA = false;
                                    break;
                                }
                            } else {
                                follow[B].insert(beta);
                                addFollowA = false;
                                break;
                            }
                        }
                        if (addFollowA) {
                            for (auto s : follow[A])
                                follow[B].insert(s);
                        }
                        size_t after = follow[B].size();
                        if (after > before)
                            changed = true;
                    }
                }
            }
        }
        follow[startSymbol].insert("$");
    }
    return follow;
}

map<string, map<string, vector<vector<string>>>> buildLL1Table(
    const map<string, vector<vector<string>>> &grammar, 
    const map<string, set<string>> &first, 
    const map<string, set<string>> &follow) 
{
    map<string, map<string, vector<vector<string>>>> table;
    for (auto &p : grammar) {
        string A = p.first;
        for (auto &prod : p.second) {
            set<string> first_alpha;
            bool derivesEpsilon = true;
            for (auto &symbol : prod) {
                if (grammar.find(symbol) != grammar.end()) {
                    for (auto s : first.at(symbol)) {
                        if (s != "ε")
                            first_alpha.insert(s);
                    }
                    if (first.at(symbol).find("ε") == first.at(symbol).end()) {
                        derivesEpsilon = false;
                        break;
                    }
                } else {
                    first_alpha.insert(symbol);
                    derivesEpsilon = false;
                    break;
                }
            }
            for (auto terminal : first_alpha) {
                table[A][terminal].push_back(prod);
            }
            if (derivesEpsilon || (prod.size() == 1 && prod[0] == "ε")) {
                for (auto terminal : follow.at(A)) {
                    table[A][terminal].push_back(prod);
                }
            }
        }
    }
    return table;
}

void printLL1Table(const map<string, map<string, vector<vector<string>>>> &table, const map<string, vector<vector<string>>> &grammar) {
    set<string> terminals;
    for (auto &p : table) {
        for (auto &entry : p.second) {
            terminals.insert(entry.first);
        }
    }
    terminals.erase("ε");
    vector<string> termList(terminals.begin(), terminals.end());
    sort(termList.begin(), termList.end());
    vector<string> nonTerminals;
    for (auto &p : table)
        nonTerminals.push_back(p.first);
    sort(nonTerminals.begin(), nonTerminals.end());

    size_t columnWidth = 20;  
    for (const string &t : termList) {
        columnWidth = max(columnWidth, t.length() + 4); 
    }     

    cout << "\nLL(1) Parsing Table\n";
    cout << setw(25) << left << "Non-Terminal/Terminal";
    for (auto &t : termList) {
        cout << setw(columnWidth) << left << ("\"" + t + "\"");
    }
    cout << "\n" << string(25 + termList.size() * columnWidth, '-') << "\n";

    for (auto &nt : nonTerminals) {
        cout << setw(25) << left << nt;
            for (auto &t : termList) {
                string cell;
                if (table.find(nt) != table.end() && table.at(nt).find(t) != table.at(nt).end()) {
                    vector<string> productions;
                    for (auto &prod : table.at(nt).at(t)) {
                        string prodStr;
                        for (auto &sym : prod) {
                            if (sym == "╬╡")
                                prodStr += "ε ";
                            else if (grammar.find(sym) == grammar.end()) { 
                                if (sym.front() == '"' && sym.back() == '"')
                                    prodStr += sym + " ";
                                 else
                                    prodStr += "\"" + sym + "\" ";
                            } else {
                                prodStr += sym + " ";
                            }
                        }
                    if (!prodStr.empty())
                        prodStr.pop_back();
                    productions.push_back(nt + " -> " + prodStr);
                    }
                // Wrapping text if too long
                cell = join(productions, " | ");
                if (cell.length() > columnWidth) {
                    cell = cell.substr(0, columnWidth - 3) + "...";  // Truncation
                }
                }
            cout << setw(columnWidth) << left << cell;
            }
        cout << "\n";
    }
}


void printSet(const map<string, set<string>> &data, const map<string, vector<vector<string>>> &grammar, const string &title) {
    cout << "\n" << title << " Sets\n" << endl;
    for (auto &p : data) {
        cout << p.first << " : { ";
        for (auto &t : p.second) {
            if (t == "╬╡")
                cout << "\"ε\" ";
            else if (grammar.find(t) == grammar.end())
                cout << "\"" << t << "\" ";
            else
                cout << t << " ";
        }
        cout << "}" << endl;
    }
}

int main() {
    auto grammar = readGrammar("grammar.txt");
    cout << "\nOriginal Grammar\n" << endl;
    printGrammar(grammar);

    auto factoredGrammar = leftFactorGrammar(grammar);
    cout << "\nGrammar After Left Factoring\n" << endl;
    printGrammar(factoredGrammar);

    auto noLeftRecGrammar = removeIndirectLeftRecursion(factoredGrammar);
    cout << "\nGrammar After Left Recursion Removal \n" << endl;
    printGrammar(noLeftRecGrammar);

    auto first = computeFirst(noLeftRecGrammar);
    printSet(first, noLeftRecGrammar, "FIRST");

    auto follow = computeFollow(noLeftRecGrammar, first);
    printSet(follow, noLeftRecGrammar, "FOLLOW");

    auto table = buildLL1Table(noLeftRecGrammar, first, follow);
    printLL1Table(table, noLeftRecGrammar);
    cout << endl;

    return 0;
}
