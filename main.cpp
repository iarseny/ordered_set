#include <iostream>
#include <vector>
#include <algorithm>
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/tree_policy.hpp>

using namespace std;
using namespace __gnu_pbds;

typedef tree<
int,
null_type,
less<int>,
rb_tree_tag,
tree_order_statistics_node_update>
ordered_set;

#define tree tree1

using ll = long long;
using pii = pair<int, int>;
using pll = pair<ll, ll>;

const int INF = 1e9;

const int N = 2e5 + 5;

ll tree[4 * N];
int ff[N];

int f(int x) {
    return (x & (-x));
}

void rv(int pos, int x) {
    for (;pos < N; pos += f(pos)) {
        ff[pos] += x;
    }
}

ll gt(int pos) {
    ll sm = 0;
    for (;pos > 0; pos -= f(pos)) {
        sm += ff[pos];
    }

    return sm;
}

void add(int v, int tl, int tr, int pos, int val) {
    if (tl == tr) {
        tree[v] = val;
        return;
    }

    int tm = (tl + tr) / 2;

    if (pos <= tm) {
        add(2 * v, tl, tm, pos, val);
    } else {
        add(2 * v + 1, tm + 1, tr, pos, val);
    }

    tree[v] = tree[2 * v] + tree[2 * v + 1];
}

ll get(int v, int tl, int tr, int l, int r) {
    if (l <= tl && tr <= r) {
        return tree[v];
    }

    if (l > tr || r < tl) {
        return 0;
    }

    int tm = (tl + tr) / 2;

    return get(2 * v, tl, tm, l, r) + get(2 * v + 1, tm + 1, tr, l, r);
}

ll spusk(int v, int tl, int tr, int x) {
    if (tl == tr) {
        return tl;
    }

    int tm = (tl + tr) / 2;

    if (tree[2 * v + 1] >= x) {
        return spusk(2 * v + 1, tm + 1, tr, x);
    }

    return spusk(2 * v, tl, tm, x - tree[2 * v + 1]);
}

ll find_inv(vector<ll> tt) {
    if (tt.empty()) {
        return 0;
    }

    int sz = (int)tt.size();

    ll res = 0;
    for (ll i = 1; i <= sz; i++) {
        res += (i - 1 - gt(tt[i - 1]));
        rv(tt[i - 1], 1);
    }

    for (ll i = 1; i <= sz; i++) {
        rv(tt[i - 1], -1);
    }

    return res;
}

void solve() {
    int n;
    cin >> n;

    ordered_set all_num;

    for (int i = 1; i <= n; i++) {
        all_num.insert(i);
    }

    for (int i = 0; i <= 4 * n; i++) {
        tree[i] = 0;
    }

    for (int i = 0; i <= n + 3; i++) {
        ff[i] = 0;
    }

    vector<pair<char, int>> prs;

    for (int i = 1; i <= n; i++) {
        char c;
        cin >> c;

        ll num;
        cin >> num;

        prs.push_back({c, num});
    }


    vector<ll> diff(n + 1);
    ll tmp = 0;

    for (int i = 1; i <= n; i++) {
        diff[i] = tmp;

        if (prs[i - 1].first == 's') {
            tmp = prs[i - 1].second;
        }
    }

    for (int i = 1; i <= n; i++) {
        add(1, 1, n, i, 1);
    }

    vector<int> ans(n + 1);

    for (int i = n; i >= 1; i--) {
        if (prs[i - 1].first == 'p') {
            add(1, 1, n, prs[i - 1].second, 0);
            ans[i] = prs[i - 1].second;
            auto it = all_num.find(prs[i - 1].second);

            if (it != all_num.end()) {
                all_num.erase(it);
            }

        } else {
            if (i == 1 || prs[i - 2].first == 'p') {
                ll d = prs[i - 1].second - diff[i];

                vector<ll> elems;

                for (int j = i - 1; j >= 1; j--) {
                    if (prs[j - 1].first == 's') break;

                    elems.push_back(prs[j - 1].second);
                }

                reverse(elems.begin(), elems.end());
                vector<int> srt_elems;
                sort(srt_elems.begin(), srt_elems.end());

                ll inv = find_inv(elems);

                d -= inv;

                for (auto j : elems) {
                    add(1, 1, n, j, 0);
                }

                for (auto j : elems) {
                    d -= get(1, 1, n, j, n);
                }

                d -= (int)elems.size();

                auto get_less = [&](int x) {
                    auto it = lower_bound(srt_elems.begin(), srt_elems.end(), x);

                    if (it == srt_elems.begin()) {
                        return 0;
                    }

                    it--;

                    return (int)(it - srt_elems.begin()) + 1;
                };


                int sz = (int)all_num.size();

                int l = 0;
                int r = sz - 1;

                ll found = -1;

                while (l <= r) {
                    int mid = (l + r) / 2;

                    ll val = (*all_num.find_by_order(mid));

                    ll know = 0;

                    if (val < n) {
                        know += get(1, 1, n, val + 1, n);
                    }

                    know -= 2ll * get_less(val);

                    if (know == d) {
                        found = mid;
                        break;
                    }

                    if (d < know) {
                        l = mid + 1;
                    } else {
                        r = mid - 1;
                    }
                }

                if (found == -1) {
                    cout << "Falied" << endl;
                }

                ll val = (*all_num.find_by_order(found));

                ans[i] = val;
                add(1, 1, n, val, 0);

                auto it = all_num.find(val);

                if (it != all_num.end()) {
                    all_num.erase(it);
                }

            } else {
                ll d = prs[i - 1].second - diff[i];
                d++;

                ll q = spusk(1, 1, n, d);

                ans[i] = q;
                add(1, 1, n, q, 0);
                auto it = all_num.find(q);

                if (it != all_num.end()) {
                    all_num.erase(it);
                }
            }
        }
    }

    for (int i = 1; i <= n; i++) {
        cout << ans[i] << ' ';
    }

    cout << '\n';
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);

    int t;
    cin >> t;

    while (t--) {
        solve();
    }

    return 0;
}
