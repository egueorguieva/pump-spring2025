import praw   # might need to pip or pip3 install praw/other packages
from prawcore import ResponseException
import pandas as pd
from datetime import datetime, timezone
import time
from tqdm import tqdm

class RateLimiter:
    def __init__(self, max_calls, period):
        self.max_calls = max_calls
        self.period = period
        self.calls = []
        self.total_calls = 0

    def call(self):
        now = time.time()
        self.calls = [call for call in self.calls if call > now - self.period]
        if len(self.calls) >= self.max_calls:
            sleep_time = self.calls[0] - (now - self.period)
            time.sleep(sleep_time)
        self.calls.append(now)
        self.total_calls += 1

    def get_total_calls(self):
        return self.total_calls

def scrape_reddit(reddit, subreddit_name, start_date, end_date, num_posts, is_test=False):
    rate_limiter = RateLimiter(max_calls=60, period=60)
    subreddit = reddit.subreddit(subreddit_name)

    posts = []
    pbar = tqdm(total=num_posts, desc="Fetching posts")
    for post in subreddit.top(time_filter="all", limit=None):
        if start_date <= post.created_utc <= end_date:
            rate_limiter.call()
            posts.append(post)
            pbar.update(1)
        if len(posts) >= num_posts:
            break
    pbar.close()

    processed_posts = []
    processed_comments = []
    pbar = tqdm(total=len(posts), desc="Processing posts and fetching comments")
    for post in posts:
        rate_limiter.call()
        post.comments.replace_more(limit=0)
        top_comments = sorted(post.comments, key=lambda c: c.score, reverse=True)[:3]
        
        post_data = {
            "post_id": post.id,
            "title": post.title,
            "score": post.score,
            "url": post.url,
            "num_comments": post.num_comments,
            "created_utc": datetime.fromtimestamp(post.created_utc, tz=timezone.utc),
            "author": str(post.author),
            "selftext": post.selftext
        }
        processed_posts.append(post_data)

        for i, comment in enumerate(top_comments, 1):
            comment_data = {
                "post_id": post.id,
                "comment_id": comment.id,
                "comment_body": comment.body,
                "comment_score": comment.score,
                "comment_author": str(comment.author),
                "comment_rank": i
            }
            processed_comments.append(comment_data)

        pbar.update(1)
    pbar.close()

    posts_df = pd.DataFrame(processed_posts)
    comments_df = pd.DataFrame(processed_comments)
    combined_df = posts_df.copy()
    for i in range(1, 4):
        comment_subset = comments_df[comments_df['comment_rank'] == i].drop('comment_rank', axis=1)
        comment_subset = comment_subset.add_prefix(f'comment{i}_')
        combined_df = pd.merge(combined_df, comment_subset, left_on='post_id', right_on=f'comment{i}_post_id', how='left')
        combined_df = combined_df.drop(f'comment{i}_post_id', axis=1)

    posts_df.to_csv(f"{'test_' if is_test else ''}posts_{subreddit_name}.csv", index=False)
    comments_df.to_csv(f"{'test_' if is_test else ''}comments_{subreddit_name}.csv", index=False)
    combined_df.to_csv(f"{'test_' if is_test else ''}combined_{subreddit_name}.csv", index=False)

    print(f"Scraped {len(processed_posts)} posts and their top comments and saved to CSVs.")
    print(f"Total API calls made (tracked internally): {rate_limiter.get_total_calls()}")

    return posts_df, comments_df, combined_df

def run():
    reddit = praw.Reddit(
        client_id="",
        client_secret="",
        user_agent="Scrapping webposts and comments from webpost)",
        username="",
        password=""
    )

    start_date = int(datetime(2022, 1, 1, tzinfo=timezone.utc).timestamp())
    end_date = int(datetime(2024, 12, 31, tzinfo=timezone.utc).timestamp())

    posts_df, comments_df, combined_df = scrape_reddit(reddit, "dating", start_date, end_date, num_posts=300, is_test=False)

    print("\nTest run completed. Sample of combined data:")
    print(combined_df.head())

if __name__ == "__main__":
        run()
