export default class Pagination {
    public limit: number;
    public offset: number;

    constructor(limit: number, offset: number) {
        this.limit = limit;
        this.offset = offset;
    }

    public getCurrentPage(): number {
        return (this.offset / this.limit);
    }

    public changePage(page: number): Pagination {
        return new Pagination(this.limit, page * this.limit);
    }

    public changeLimit(limit: number): Pagination {
        return new Pagination(limit, 0);
    }

    public advancePage(): Pagination {
        return this.changePage(this.getCurrentPage() + 1);
    }
}
