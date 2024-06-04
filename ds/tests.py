import math
import unittest
from operator import add

from segment_tree import SegmentTree4N


class TestSegmentTree4N(unittest.TestCase):
    def setUp(self):
        self.sample_data = [1, 3, 5, 7, 9, 11]

    def test_min_query(self):
        st = SegmentTree4N(self.sample_data, op=min, default=math.inf)
        self.assertEqual(st.query(0, 2), 1)  # min(1, 3, 5)
        self.assertEqual(st.query(1, 3), 3)  # min(3, 5, 7)
        self.assertEqual(st.query(0, 5), 1)  # min(1, 3, 5, 7, 9, 11)

    def test_sum_query(self):
        st = SegmentTree4N(self.sample_data, op=add, default=0)
        self.assertEqual(st.query(0, 2), 9)  # sum(1, 3, 5)
        self.assertEqual(st.query(1, 3), 15)  # sum(3, 5, 7)
        self.assertEqual(st.query(0, 5), 36)  # sum(1, 3, 5, 7, 9, 11)

    def test_max_query(self):
        st = SegmentTree4N(self.sample_data, op=max, default=-math.inf)
        self.assertEqual(st.query(0, 2), 5)  # max(1, 3, 5)
        self.assertEqual(st.query(1, 3), 7)  # max(3, 5, 7)
        self.assertEqual(st.query(0, 5), 11)  # max(1, 3, 5, 7, 9, 11)

    def test_update(self):
        st = SegmentTree4N(self.sample_data, op=min, default=math.inf)
        st.update(1, 0)  # update index 1 to value 0
        self.assertEqual(st.query(0, 2), 0)  # min(1, 0, 5)
        self.assertEqual(st.query(1, 3), 0)  # min(0, 5, 7)
        self.assertEqual(st.query(0, 5), 0)  # min(1, 0, 5, 7, 9, 11)

    def test_combined_operations(self):
        st = SegmentTree4N(self.sample_data, op=min, default=math.inf)
        self.assertEqual(st.query(0, 5), 1)  # min(1, 3, 5, 7, 9, 11)
        st.update(3, 0)  # update index 3 to value 0
        self.assertEqual(st.query(0, 5), 0)  # min(1, 3, 5, 0, 9, 11)
        st.update(0, 6)  # update index 0 to value 6
        self.assertEqual(st.query(0, 5), 0)  # min(6, 3, 5, 0, 9, 11)


if __name__ == "__main__":
    unittest.main()
